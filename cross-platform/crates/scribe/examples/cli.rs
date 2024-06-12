use std::{
    fs, io,
    path::{Path, PathBuf},
};

#[derive(argh::FromArgs)]
#[argh(description = "")]
struct Args {
    #[argh(subcommand)]
    command: Command,
}

#[derive(argh::FromArgs)]
#[argh(subcommand)]
enum Command {
    Compress(Compress),
    Decompress(Decompress),
}

#[derive(argh::FromArgs)]
#[argh(description = "")]
#[argh(subcommand, name = "compress")]
struct Compress {
    #[argh(positional)]
    #[argh(description = "input")]
    input: PathBuf,
    #[argh(option, short = 'o')]
    #[argh(description = "output")]
    output: PathBuf,
}

#[derive(argh::FromArgs)]
#[argh(description = "")]
#[argh(subcommand, name = "decompress")]
struct Decompress {
    #[argh(positional)]
    #[argh(description = "input")]
    input: PathBuf,
    #[argh(option, short = 'o')]
    #[argh(description = "output")]
    output: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args: Args = argh::from_env();

    match args.command {
        Command::Compress(args) => compress(args),
        Command::Decompress(args) => decompress(args),
    }
}

use scribe::{v104, Archive, Entry};

fn compress(args: Compress) -> anyhow::Result<()> {
    Ok(())
}

fn decompress(args: Decompress) -> anyhow::Result<()> {
    fn read_and_write<A: Archive>(
        parent: &Path,
        archive: &mut A,
        mut folder: A::FolderPtr,
    ) -> anyhow::Result<()>
    where
        anyhow::Error: From<A::Err>,
    {
        let path_ext = archive.folder_path(&folder)?.unwrap();
        let proper_path_ext = <PathBuf as path_slash::PathBufExt>::from_backslash(&path_ext);
        let path = parent.join(proper_path_ext);

        fs::create_dir_all(&path)?;

        while let Some(entry) = archive.next_entry(&mut folder)? {
            match entry {
                Entry::Folder(folder) => read_and_write(&path, archive, folder)?,
                Entry::File(file) => {
                    let file_name = archive.file_name(&file)?.unwrap();
                    let full_path = path.join(file_name);

                    let mut out = fs::OpenOptions::new()
                        .write(true)
                        .create(true)
                        .open(&full_path)?;

                    let mut contents = archive.read_file_contents(&file)?;

                    io::copy(&mut contents, &mut out)?;
                }
            }
        }

        Ok(())
    }

    let file = fs::File::open(&args.input)?;

    let (mut archive, ..) = v104::read(file)?;

    let root = archive.root_folder()?;
    read_and_write(&args.output, &mut archive, root)?;

    Ok(())
}
