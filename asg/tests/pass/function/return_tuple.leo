// Returns a tuple of tuples.
function tuples() -> ((u8, u8), u32) {
    let a: (u8, u8) = (1, 2);
    let b: u32 = 3;

    return (a, b)
}

function main() {
    let t = tuples();
}