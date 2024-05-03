type ModeT = u32;

mod values {
    pub const S_IFMT: u32 = 0o00170000;
    pub const S_IFSOCK: u32 = 0o0140000;
    pub const S_IFLNK: u32 = 0o0120000;
    pub const S_IFREG: u32 = 0o0100000;
    pub const S_IFBLK: u32 = 0o0060000;
    pub const S_IFDIR: u32 = 0o0040000;
    pub const S_IFCHR: u32 = 0o0020000;
    pub const S_IFIFO: u32 = 0o0010000;
    pub const S_ISUID: u32 = 0o0004000;
    pub const S_ISGID: u32 = 0o0002000;
    pub const S_ISVTX: u32 = 0o0001000;

    pub const S_IRWXU: u32 = 0o00700;
    pub const S_IRUSR: u32 = 0o00400;
    pub const S_IWUSR: u32 = 0o00200;
    pub const S_IXUSR: u32 = 0o00100;

    pub const S_IRWXG: u32 = 0o00070;
    pub const S_IRGRP: u32 = 0o00040;
    pub const S_IWGRP: u32 = 0o00020;
    pub const S_IXGRP: u32 = 0o00010;

    pub const S_IRWXO: u32 = 0o00007;
    pub const S_IROTH: u32 = 0o00004;
    pub const S_IWOTH: u32 = 0o00002;
    pub const S_IXOTH: u32 = 0o00001;
}

macro_rules! bitflags {
    (
        $(#[$outer:meta])*
        pub struct $BitFlags:ident: $T:ty {
            $(
                $(#[$inner:ident $($args:tt)*])*
                $Flag:ident $(as $cast:ty)*;
            )+
        }
    ) => {
        ::bitflags::bitflags! {
            #[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            #[repr(transparent)]
            $(#[$outer])*
            pub struct $BitFlags: $T {
                $(
                    $(#[$inner $($args)*])*
                    const $Flag = values::$Flag $(as $cast)*;
                )+
            }
        }
    };
}

bitflags!(
    /// "File type" flags for `mknod` and related functions.
    pub struct SFlag: ModeT {
        S_IFIFO;
        S_IFCHR;
        S_IFDIR;
        S_IFBLK;
        S_IFREG;
        S_IFLNK;
        S_IFSOCK;
        S_IFMT;
    }
);

bitflags! {
    /// "File mode / permissions" flags.
    pub struct Mode: ModeT {
        /// Read, write and execute for owner.
        S_IRWXU;
        /// Read for owner.
        S_IRUSR;
        /// Write for owner.
        S_IWUSR;
        /// Execute for owner.
        S_IXUSR;
        /// Read write and execute for group.
        S_IRWXG;
        /// Read for group.
        S_IRGRP;
        /// Write for group.
        S_IWGRP;
        /// Execute for group.
        S_IXGRP;
        /// Read, write and execute for other.
        S_IRWXO;
        /// Read for other.
        S_IROTH;
        /// Write for other.
        S_IWOTH;
        /// Execute for other.
        S_IXOTH;
        /// Set user id on execution.
        S_ISUID;
        /// Set group id on execution.
        S_ISGID;
        S_ISVTX;
    }
}
