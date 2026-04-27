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

//! Tests for interface ABI generation (`generate_program_interfaces`).

use leo_abi::interfaces::{CompiledInterface, InterfaceOwner};
use leo_ast::NodeBuilder;
use leo_errors::Handler;
use leo_span::{Symbol, create_session_if_not_set_then};

use indexmap::IndexMap;
use serial_test::serial;
use std::rc::Rc;

/// Compile a multi-program source (separated by `PROGRAM_DELIMITER`) and return
/// the interface ABIs from the final program.
///
/// Dependencies are parsed as Leo stubs (FromLeo mode), following the same
/// protocol as `test_compiler.rs`.
fn compile_interfaces(source: &str) -> Vec<CompiledInterface> {
    let handler = Handler::default();
    let node_builder = Rc::new(NodeBuilder::default());
    let mut import_stubs = IndexMap::new();

    let sources: Vec<&str> = source.split(super::test_utils::PROGRAM_DELIMITER).collect();
    let (last, rest) = sources.split_last().expect("non-empty sources");

    // Parse each dependency as a Leo program stub.
    for dep_source in rest {
        let (program, program_name) =
            super::test_utils::parse_program(dep_source, &handler, &node_builder, import_stubs.clone())
                .unwrap_or_else(|e| panic!("dependency compilation failed: {e}"));
        import_stubs.insert(Symbol::intern(&program_name), program.into());
    }

    // Full compile for the final program.
    let (compiled, _name) = super::test_utils::whole_compile(last, &handler, &node_builder, import_stubs)
        .unwrap_or_else(|e| panic!("compilation failed: {e}"));
    compiled.interfaces
}

/// Helper: find a `CompiledInterface` by name.
fn find_by_name<'a>(interfaces: &'a [CompiledInterface], name: &str) -> Option<&'a CompiledInterface> {
    interfaces.iter().find(|ci| ci.abi.name == name)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

/// A program implementing an external interface whose parent is in a third
/// external program must emit ABIs for both the child and the grandparent.
#[test]
#[serial]
fn external_interface_transitive_parent() {
    create_session_if_not_set_then(|_| {
        let source = "\
interface Base {
    fn get() -> u64;
}

program lib_a.aleo {
    fn dummy() {}
}
// --- Next Program --- //
import lib_a.aleo;

interface Extended: lib_a.aleo::Base {
    fn set(v: u64) -> u64;
}

program lib_b.aleo {
    fn dummy() {}
}
// --- Next Program --- //
import lib_a.aleo;
import lib_b.aleo;

program test.aleo: lib_b.aleo::Extended {
    fn get() -> u64 {
        return 0u64;
    }

    fn set(v: u64) -> u64 {
        return v;
    }
}
";
        let interfaces = compile_interfaces(source);

        let extended = find_by_name(&interfaces, "Extended").expect("Extended ABI should be emitted");
        assert!(matches!(&extended.owner, InterfaceOwner::External { owner_program } if owner_program == "lib_b.aleo"));
        assert_eq!(extended.abi.parents.len(), 1, "Extended should have one parent ref");

        let base = find_by_name(&interfaces, "Base").expect("Base ABI should be emitted (transitive parent)");
        assert!(matches!(&base.owner, InterfaceOwner::External { owner_program } if owner_program == "lib_a.aleo"));
    });
}

/// A locally-defined interface extending an external interface must cause the
/// external parent's ABI to be emitted alongside the local one.
#[test]
#[serial]
fn local_interface_with_external_parent() {
    create_session_if_not_set_then(|_| {
        let source = "\
interface Base {
    fn get() -> u64;
}

program lib.aleo {
    fn unused() {}
}
// --- Next Program --- //
import lib.aleo;

interface Extended: lib.aleo::Base {
    fn set(v: u64) -> u64;
}

program test.aleo: Extended {
    fn get() -> u64 {
        return 0u64;
    }

    fn set(v: u64) -> u64 {
        return v;
    }
}
";
        let interfaces = compile_interfaces(source);

        let extended = find_by_name(&interfaces, "Extended").expect("Extended ABI should be emitted");
        assert!(matches!(&extended.owner, InterfaceOwner::Local));

        let base =
            find_by_name(&interfaces, "Base").expect("Base ABI should be emitted (external parent of local interface)");
        assert!(matches!(&base.owner, InterfaceOwner::External { owner_program } if owner_program == "lib.aleo"));
    });
}

/// Diamond inheritance across programs: both paths lead to the same grandparent
/// which should be emitted exactly once.
#[test]
#[serial]
fn diamond_external_parent_emitted_once() {
    create_session_if_not_set_then(|_| {
        let source = "\
interface Root {
    fn root_fn() -> u64;
}

program root.aleo {
    fn dummy() {}
}
// --- Next Program --- //
import root.aleo;

interface Left: root.aleo::Root {
    fn left_fn() -> u64;
}

program left.aleo {
    fn dummy() {}
}
// --- Next Program --- //
import root.aleo;

interface Right: root.aleo::Root {
    fn right_fn() -> u64;
}

program right.aleo {
    fn dummy() {}
}
// --- Next Program --- //
import root.aleo;
import left.aleo;
import right.aleo;

program test.aleo: left.aleo::Left + right.aleo::Right {
    fn root_fn() -> u64 {
        return 0u64;
    }

    fn left_fn() -> u64 {
        return 1u64;
    }

    fn right_fn() -> u64 {
        return 2u64;
    }
}
";
        let interfaces = compile_interfaces(source);

        assert!(find_by_name(&interfaces, "Left").is_some(), "Left should be emitted");
        assert!(find_by_name(&interfaces, "Right").is_some(), "Right should be emitted");

        let root_count = interfaces.iter().filter(|ci| ci.abi.name == "Root").count();
        assert_eq!(root_count, 1, "Root should be emitted exactly once (diamond dedup)");
    });
}
