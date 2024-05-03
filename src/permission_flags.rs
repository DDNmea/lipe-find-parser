use libc::mode_t;

macro_rules! libc_bitflags {
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
                    const $Flag = libc::$Flag $(as $cast)*;
                )+
            }
        }
    };
}

libc_bitflags!(
    /// "File type" flags for `mknod` and related functions.
    pub struct SFlag: mode_t {
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

libc_bitflags! {
    /// "File mode / permissions" flags.
    pub struct Mode: mode_t {
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
        S_ISUID as mode_t;
        /// Set group id on execution.
        S_ISGID as mode_t;
        S_ISVTX as mode_t;
    }
}
