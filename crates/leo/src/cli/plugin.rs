// Copyright (C) 2019-2026 Provable Inc.
// This file is part of the Leo library.

// The Leo library is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// The Leo library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with the Leo library. If not, see <https://www.gnu.org/licenses/>.

use leo_errors::{CliError, Result};

use std::{
    convert::Infallible,
    ffi::OsString,
    path::{Path, PathBuf},
    process,
};

/// Scan `PATH` for an executable named `name`.
pub fn find_exe(name: &str) -> Option<PathBuf> {
    let var = std::env::var_os("PATH")?;
    std::env::split_paths(&var).find_map(|dir| {
        let candidate = dir.join(name);
        if is_executable(&candidate) { Some(candidate) } else { None }
    })
}

/// Find and execute a plugin binary, forwarding `args` and optionally setting
/// the working directory to `cwd`.
///
/// On Unix this replaces the current process via `exec`. On other platforms it
/// spawns the plugin and propagates the exit code.
pub fn exec(name: &str, args: &[OsString], cwd: Option<&Path>) -> Result<Infallible> {
    let path = find_exe(name).ok_or_else(|| -> leo_errors::LeoError {
        CliError::custom(format!("'{name}' not found. Install the plugin and ensure it is available on your PATH."))
            .into()
    })?;

    let mut cmd = process::Command::new(&path);
    cmd.args(args);
    if let Some(dir) = cwd {
        cmd.current_dir(dir);
    }

    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        // Does not return on success.
        let err = cmd.exec();
        Err(CliError::custom(format!("failed to exec '{name}': {err}")).into())
    }

    #[cfg(not(unix))]
    {
        let status = cmd
            .stdin(process::Stdio::inherit())
            .stdout(process::Stdio::inherit())
            .stderr(process::Stdio::inherit())
            .status()
            .map_err(|err| -> leo_errors::LeoError {
                CliError::custom(format!("failed to spawn '{name}': {err}")).into()
            })?;
        process::exit(status.code().unwrap_or(1));
    }
}

#[cfg(unix)]
fn is_executable(path: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;
    path.is_file() && std::fs::metadata(path).map(|m| m.permissions().mode() & 0o111 != 0).unwrap_or(false)
}

#[cfg(not(unix))]
fn is_executable(path: &Path) -> bool {
    path.is_file()
}
