circuit Foo {
    a: u8,

    function set_a(self, new: u8) {
        self.a = new;
    }
}

function main() {
    let mut f  = Foo { a: 0u8 };

    f.set_a(1u8);
}