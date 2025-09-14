use criterion::{
    Criterion, Throughput, criterion_group, criterion_main,
};
use ehc::lexing::*;
use ehc::preprocessing::*;
use std::fs;
use std::hint::black_box;

fn lex_benchmark(c: &mut Criterion) {
    let bytes = fs::read("benches/data/bench.c").expect("data/bench.c not found");
    let text = preprocess(bytes.clone()).unwrap();
    let size_bytes = bytes.len() as u64;

    let mut group = c.benchmark_group("tokenise");
    group.throughput(Throughput::Bytes(size_bytes));

    group.bench_function("tokenise", |b| {
        b.iter(|| {
            let mut tokeniser = Tokeniser::new(black_box(text.as_str()));
            for _tok in &mut tokeniser {
                _ = black_box(_tok.unwrap());
            }
        });
    });

    group.finish();
}

criterion_group!(benches, lex_benchmark);
criterion_main!(benches);
