use blob_common::{VERSION_MAJOR, VERSION_MINOR};

pub const BLOB_EXECUTABLE_FILE_EXTENTION: &'static str = "blobexec";
pub const BLOB_EXECUTABLE_HEADER_SIZE: usize = 64;
pub const BLOB_EXECUTABLE_HEADER_PADDING: usize = BLOB_EXECUTABLE_HEADER_SIZE - 6;
pub const BLOB_HEADER_MAGIC_NUMBER: [u8; 4] = [43, 67, 88, 145];

#[repr(C)]
pub struct BlobExecutable {
    header: BlobExecutableHeader,
    code_start: u64,
    program: Vec<u8>,
}

impl BlobExecutable {
    pub fn new(code_start: u64, program: Vec<u8>) -> BlobExecutable {
        BlobExecutable {
            header: BlobExecutableHeader::new(),
            code_start,
            program,
        }
    }

    pub fn load_from_file(_file_name: &str) -> Result<BlobExecutable, ()> {
        todo!()
    }

    pub fn save(&self, _file_name: &str) -> Result<(), ()> {
        todo!()
    }

    pub fn get_code_start(&self) -> usize {
        self.code_start as usize
    }

    pub fn get_program(&self) -> &Vec<u8> {
        &self.program
    }
}

#[repr(C)]
struct BlobExecutableHeader {
    magic_number: [u8; 4],
    version_major: u8,
    version_minor: u8,
    _padding: [u8; BLOB_EXECUTABLE_HEADER_PADDING],
}

impl BlobExecutableHeader {
    pub fn new() -> BlobExecutableHeader {
        BlobExecutableHeader {
            magic_number: BLOB_HEADER_MAGIC_NUMBER,
            version_major: VERSION_MAJOR,
            version_minor: VERSION_MINOR,
            _padding: [0; BLOB_EXECUTABLE_HEADER_PADDING],
        }
    }
}
