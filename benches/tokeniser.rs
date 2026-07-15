use asjcc::preprocessing::*;
use asjcc::tokenising::*;
use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use std::fs;
use std::hint::black_box;

fn tokenise_benchmark(c: &mut Criterion) {
    let bytes =
        fs::read("benches/data/sqlite3.c").expect("data/sqlite3.c not found");
    let text = preprocess(bytes.clone()).unwrap();
    let size_bytes = bytes.len() as u64;

    let mut group = c.benchmark_group("tokenise");
    group.throughput(Throughput::Bytes(size_bytes));
    group.sample_size(500);

    group.bench_function("tokenise", |b| {
        b.iter(|| {
            let mut tokeniser = Tokeniser::new(black_box(text.as_str()));
            for _tok in tokeniser.iter() {
                _ = black_box(_tok.unwrap());
            }
        });
    });

    group.finish();
}

criterion_group!(benches, tokenise_benchmark);
criterion_main!(benches);
