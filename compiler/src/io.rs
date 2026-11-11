use std::{iter, vec};

use itertools::join;
use unicode_width::UnicodeWidthStr;

use crate::common::CodeLocation;



pub fn format_error(input: &String, loc: CodeLocation, error_type: &str, message: &str) -> String{


    /* Looks like this:
        [destination]nodeStatus = InFrontier;
                                  ^^^^^^^^^^
     */
    let mut error_location_lines: Vec<String> = vec![];
    let mut line_at_start = 0;
    let mut col_at_start: i32 = 0;

    let mut line = String::new();
    let mut carets = String::new();
    let mut line_num = 1;
    let mut col_num = 0;
    for (i, ch) in input.char_indices() {
        col_num += 1;
        
        // error begins on this line. Add spaces up to the start of the error
        if i == loc.start_index {
            line_at_start = line_num;
            col_at_start = col_num;
            carets = iter::repeat(" ").take(UnicodeWidthStr::width(line.as_str())).collect::<String>();
        }

        if ch == '\n' {
            // if end of a line and still inside the error, add carets til the end of the line
            if i >= loc.start_index && i < loc.end_index {
                let num_carets_to_add = UnicodeWidthStr::width(line.as_str()) - carets.len();
                carets += &iter::repeat("^").take(num_carets_to_add).collect::<String>();
            }
            if i >= loc.start_index {
                error_location_lines.push(line);
                error_location_lines.push(carets);
            }
            carets = String::new();
            line = String::new();
            line_num += 1;
            col_num = 0;
            if i >= loc.end_index {
                break;
            }
        } else {
            line += &ch.to_string();
        }

        // at end of error section. Add carets
        if i == loc.end_index - 1 {
            let num_carets_to_add = UnicodeWidthStr::width(line.as_str()) - carets.len();
            carets += &iter::repeat("^").take(num_carets_to_add).collect::<String>();
        }
    }

    // if loop terminated by reaching EOF
    if carets.len() != 0 {
        error_location_lines.push(line);
        error_location_lines.push(carets);
    }
    
    return format!("Line {}, Col {}: {}\n{}\n{}", 
        line_at_start, col_at_start, error_type, join(error_location_lines, "\n"), message
    )

}