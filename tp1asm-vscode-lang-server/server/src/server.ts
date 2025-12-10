
// Code copied with alterations from lsp-sample provided by Microsoft.

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	CompletionItem,
	CompletionItemKind,
	TextDocumentSyncKind,
	InitializeResult,
	DocumentDiagnosticReportKind,
	type DocumentDiagnosticReport,
	MarkupKind
} from 'vscode-languageserver/node';

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

import {access} from "fs/promises";
import { constants } from 'fs';
import { spawn } from 'child_process';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
// let hasDiagnosticRelatedInformationCapability = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	// Does the client support the `workspace/configuration` request?
	// If not, we fall back using global settings.
	hasConfigurationCapability = !!(
		capabilities.workspace && !!capabilities.workspace.configuration
	);
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);
	// hasDiagnosticRelatedInformationCapability = !!(
	// 	capabilities.textDocument &&
	// 	capabilities.textDocument.publishDiagnostics &&
	// 	capabilities.textDocument.publishDiagnostics.relatedInformation
	// );

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// Tell the client that this server supports code completion.
			completionProvider: {
				resolveProvider: true
			},
			diagnosticProvider: {
				interFileDependencies: false,
				workspaceDiagnostics: false
			},
			hoverProvider: true,
		}
	};
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}
	return result;
});

connection.onInitialized(() => {
	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, undefined);
	}
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
		});
	}
});

// The example settings
interface Tp1AsmSettings {
	assemblerExecutablePath: string | undefined;
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: Tp1AsmSettings = { assemblerExecutablePath: undefined};
let globalSettings: Tp1AsmSettings = defaultSettings;

// Cache the settings of all open documents
const documentSettings = new Map<string, Thenable<Tp1AsmSettings>>();
const documentInfos = new Map<string, TP1DocumentInfo>();

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		documentSettings.clear();
	} else {
		globalSettings = (
			(change.settings.tp1asmLanguageServer || defaultSettings)
		);
	}
	// Refresh the diagnostics since the `maxNumberOfProblems` could have changed.
	// We could optimize things here and re-fetch the setting first can compare it
	// to the existing setting, but this is out of scope for this example.
	for (const doc of documents.all()) {
		refreshTP1DocumentInfo(doc);
	}
});

function getDocumentSettings(resource: string): Thenable<Tp1AsmSettings> {
	if (!hasConfigurationCapability) {
		return Promise.resolve(globalSettings);
	}
	let result = documentSettings.get(resource);
	if (!result) {
		result = connection.workspace.getConfiguration({
			scopeUri: resource,
			section: 'tp1asmLanguageServer'
		});
		documentSettings.set(resource, result);
	}
	return result;
}

// Only keep settings for open documents
documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
	documentInfos.delete(e.document.uri);
});


connection.languages.diagnostics.on(async (params) => {
	const document = documents.get(params.textDocument.uri);
	if (document !== undefined) {

		const info = documentInfos.get(document.uri);
		const diagnosticItems = info ? infoToDiagnostics(document, info) : [];

		return {
			kind: DocumentDiagnosticReportKind.Full,
			items: diagnosticItems
		} satisfies DocumentDiagnosticReport;
	} else {
		// We don't know the document. We can either try to read it from disk
		// or we don't report problems for it.
		return {
			kind: DocumentDiagnosticReportKind.Full,
			items: []
		} satisfies DocumentDiagnosticReport;
	}
});

connection.onHover(({position, textDocument}) => {
	// check if we are hovering on a label
	const document = documents.get(textDocument.uri);
	if (!document) {
		return null;
	}
	const line = document.getText().split("\n")[position.line];

	// check not comment
	const commentStartPos = line.search(";");
	if (commentStartPos != -1 && position.character >= commentStartPos) {
		return null;
	}

	const pattern = /[a-z][a-zA-Z]*/g;

	let array: RegExpExecArray | null;

	while ((array = pattern.exec(line)) !== null) {
		const lowerBound = pattern.lastIndex - array[0].length;
		if (position.character < lowerBound) {
			return null;
		}
		const upperBound = pattern.lastIndex;
		if (position.character < upperBound) {
			const label = array[0];
			const value = documentInfos.get(textDocument.uri)?.labels.get(label);
			if (value === undefined) {
				return null;
			}
			return {
				contents: {
					kind: MarkupKind.Markdown,
					value: `\`\`\`tp1asm
${label} = ${value}
\`\`\``
				},
			};
		}
	}

	return null;
});

connection.onCompletion(({textDocument}) => {
	const registers = ["D0", "D1", "{D0}", "PC", "IN"];
	const registerCompletions: CompletionItem[] = registers.map(register =>({
		label: register,
		kind: CompletionItemKind.Constant,
	}));

	const info = documentInfos.get(textDocument.uri);
	const labelCompletions: CompletionItem[] = info? [...info.labels.keys()].map(label => ({
		label: label,
		kind: CompletionItemKind.Variable,
	})) : [];

	return [
		...registerCompletions,
		...labelCompletions,
	];
});


connection.onCompletionResolve(
	(item: CompletionItem): CompletionItem => {
		// if (item.data === 1) {
		// 	item.detail = 'TypeScript details';
		// 	item.documentation = 'TypeScript documentation';
		// } else if (item.data === 2) {
		// 	item.detail = 'JavaScript details';
		// 	item.documentation = 'JavaScript documentation';
		// }
		return item;
	}
);


interface TP1DocumentInfo {
	labels: Map<string, number>,
	errors: {
		line: number | undefined,
		detail:string
	}[],
};

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(async change => {
	await refreshTP1DocumentInfo(change.document);
});


async function refreshTP1DocumentInfo(document: TextDocument) {
	let info: TP1DocumentInfo;
	try {
		info = await getTP1DocumentInfo(document);
	} catch (e) {
		console.error(e);
		return;
	}
	documentInfos.set(document.uri, info);
	connection.languages.diagnostics.refresh();
}

async function getTP1DocumentInfo(textDocument: TextDocument): Promise<TP1DocumentInfo> {

	// could error.
	const {stdout, stderr} = await getAssemblerOutput(textDocument);

	const errors = extractErrorsFromAssemblerStderr(stderr);
	const labels = extractLabelMapFromAssemblerStdout(stdout);

	return {errors, labels};


}

function extractLabelMapFromAssemblerStdout(stdout: string[]) {
	const labels = new Map<string, number>();
	const pattern = /^Label: ([^:]*): ([0-9]+)/;
	for (const item of stdout) {
		for (const line of item.split("\n")) {
			const match = line.match(pattern);
			if (!match) {
				continue;
			}
			const label = match[1];
			const value = parseInt(match[2]);
			if (isNaN(value)) {
				continue;
			}
			labels.set(label, value);
		}
	}
	return labels;
}

function extractErrorsFromAssemblerStderr(stderr: string[]) {

	const errors: TP1DocumentInfo["errors"] = [];
	const pattern = /^Error:(?: line ([0-9]+):)? (.*)$/;

	for (const error of stderr) {
		for (const line of error.split("\n")) {
			const match = line.match(pattern);
			if (!match) {continue;}

			const lineNumber = match[1] ? parseInt(match[1]) : undefined;
			const message = match[2];

			errors.push({
				line: lineNumber,
				detail: message
			});
		}

	}
	return errors;
}

function infoToDiagnostics(textDocument: TextDocument, info: TP1DocumentInfo): Diagnostic[] {
	
	const text = textDocument.getText();
	const diagnostics: Diagnostic[] = [];

	for (const {line, detail} of info.errors) {

		let range: Diagnostic["range"];
		if (line) {
			// find non-whitespace part of the line
			let i = textDocument.offsetAt({line: line-1, character: 0});
			while (i < text.length && /\s/.test(text[i])) {
				i++;
			}
			const textStartOffset = i;
			i = textDocument.offsetAt({line: line, character: 0}) - 1;
			while (i > textStartOffset && /\s/.test(text[i])) {
				i--;
			}
			const textEndOffset = i + 1;
			range = {
				start: textDocument.positionAt(textStartOffset),
				end: textDocument.positionAt(textEndOffset),
			};
		} else {
			// just highlight the entire file
			range = {
				start: textDocument.positionAt(0),
				end: textDocument.positionAt(text.length)
			};
		}

		const diagnostic: Diagnostic = {
			severity: DiagnosticSeverity.Error,
			range: range,
			message: detail,
			source: "tp1asm"
		};

		diagnostics.push(diagnostic);
	}

	return diagnostics;
}


function getAssemblerOutput(textDocument: TextDocument): Promise<{stdout: string[], stderr: string[]}> {
	return new Promise((resolve, reject) => {
		(async () => {
			const settings = await getDocumentSettings(textDocument.uri);
	
			// no assembler path provided
			if (!settings.assemblerExecutablePath) {
				return reject("No path to assembler provided");
			}
	
			try {
				await access(settings.assemblerExecutablePath, constants.X_OK);
			} catch {
				return reject("Assembler not found, or is not executable");
			}
	
			const stdout: string[] = [];
			const stderr: string[] = [];
	
			const assemblerProcess = spawn(settings.assemblerExecutablePath, [settings.assemblerExecutablePath]);
			


			const killTimer = setTimeout(() => {
				assemblerProcess.kill();
				reject("Assembler timed out");
			}, 5000);

			assemblerProcess.on("close", () => {
				clearTimeout(killTimer);
				resolve({stdout, stderr});
			});

			assemblerProcess.stdout.on('data', (data) => {
				stdout.push((data as Buffer).toString());
			});

			assemblerProcess.stderr.on('data', (data) => {
				stderr.push((data as Buffer).toString());
			});
	
			assemblerProcess.stdin.write(textDocument.getText());
			assemblerProcess.stdin.end();

		})();
	});
}


connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received a file change event');
});

// // This handler provides the initial list of the completion items.
// connection.onCompletion(
// 	(_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
// 		// The pass parameter contains the position of the text document in
// 		// which code complete got requested. For the example we ignore this
// 		// info and always provide the same completion items.
// 		return [
// 			{
// 				label: 'TypeScript',
// 				kind: CompletionItemKind.Text,
// 				data: 1
// 			},
// 			{
// 				label: 'JavaScript',
// 				kind: CompletionItemKind.Text,
// 				data: 2
// 			}
// 		];
// 	}
// );

// // This handler resolves additional information for the item selected in
// // the completion list.
// connection.onCompletionResolve(
// 	(item: CompletionItem): CompletionItem => {
// 		if (item.data === 1) {
// 			item.detail = 'TypeScript details';
// 			item.documentation = 'TypeScript documentation';
// 		} else if (item.data === 2) {
// 			item.detail = 'JavaScript details';
// 			item.documentation = 'JavaScript documentation';
// 		}
// 		return item;
// 	}
// );

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
