
#[derive(Default, Clone, Copy, Debug)]
pub struct CodeLocation {
    pub start_index: usize,
    pub end_index: usize

}

#[derive(Debug, Clone, Copy)]
pub enum BranchFlag {
    BZ = 0,
    BNZ = 1,
    BC = 2,
    BNC = 3,
    BN = 4,
    BNN = 5,
    BO = 6,
    BNO = 7,
    BGTE = 8,
    BNGTE = 9,
}