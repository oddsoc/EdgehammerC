// ==============================
// 1. Single-element array
// ==============================
int a1[1] = { 1 };

// ==============================
// 2. Simple fixed-size array
// ==============================
int a2[3] = { 1, 2, 3 };

// ==============================
// 3. Fixed-size array with trailing comma
// ==============================
int a3[3] = { 1, 2, 3, };

// ==============================
// 4. Fixed-size array, partial initialiser
// Remaining elements should be zero
// ==============================
int a4[5] = { 1, 2 };

// ==============================
// 5. Flexible array (size omitted)
// ==============================
int a5[] = { 1, 2, 3 };

// ==============================
// 6. Flexible array with trailing comma
// ==============================
int a6[] = { 1, 2, 3, };

// ==============================
// 7. Simple 2D array with full braces
// ==============================
int a7[2][2] = {
    { 1, 2 },
    { 3, 4 }
};

// ==============================
// 8. 2D array with brace elision
// (array_initialiser2 should kick in)
// ==============================
int a8[2][2] = { 1, 2, 3, 4 };

// ==============================
// 9. 2D array with mixed braces
// ==============================
int a9[2][2] = {
    1, 2,
    { 3, 4 }
};

// ==============================
// 10. Flexible outer array, fixed inner array
// ==============================
int a10[][2] = { 1, 2, 3, 4 };

// ==============================
// 11. Flexible outer array with explicit braces
// ==============================
int a11[][2] = {
    { 1, 2 },
    { 3, 4 }
};

// ==============================
// 12. Deeply nested array, brace elision
// ==============================
int a12[2][2][2] = {
    1, 2, 3, 4,
    5, 6, 7, 8
};


// ==============================
// 14. Designated initialiser (NOT IMPLEMENTED YET)
// ==============================
// int a14[5] = { [2] = 10 };

// ==============================
// 15. Designated initialiser with range (NOT IMPLEMENTED YET)
// ==============================
// int a15[5] = { [1 ... 3] = 7 };

// ==============================
// 16. Mixed designated and non-designated (NOT IMPLEMENTED YET)
// ==============================
// int a16[5] = { 1, [3] = 4, 5 };

// ==============================
// 17. Designated initialiser in nested array (NOT IMPLEMENTED YET)
// ==============================
// int a17[2][2] = { [1][0] = 5 };

// ==============================
// 18. Flexible 2D: 3x3 with full brace elision
// Should count: 3 arrays of [3]int
// ==============================
int a18[][3] = {
    1, 2, 3,
    4, 5, 6,
    7, 8, 9
};

// ==============================
// 19. Flexible 2D: mixed brace styles
// 2 arrays of [3]int: one explicit, one elided
// ==============================
int a19[][3] = {
    { 1, 2, 3 },
    4, 5, 6
};

// ==============================
// 20. Flexible 3D with full elision: 2x2x2
// Should count 2 arrays of [2][2]int
// ==============================
int a20[][2][2] = {
    1, 2, 3, 4,
    5, 6, 7, 8
};

// ==============================
// 21. Flexible 3D with mixed elision: 2x2x2
// First level explicit, second level elided
// Should count 2 arrays of [2][2]int
// ==============================
int a21[][2][2] = {
    { 1, 2, 3, 4 },
    { 5, 6, 7, 8 }
};

// ==============================
// 22. Flexible 3D with full explicit braces
// Should count 2 arrays of [2][2]int
// ==============================
int a22[][2][2] = {
    { { 1, 2 }, { 3, 4 } },
    { { 5, 6 }, { 7, 8 } }
};

// ==============================
// 23. Tricky: Flexible outer, then fixed [3], then flexible inner
// Should count the flexible outermost dimension
// int a23[][3][] - NOT VALID C (can't have flexible inner)
// ==============================

// ==============================
// 24. Large flexible array with single initialiser
// ==============================
int a24[] = { 42 };

// ==============================
// 25. Flexible 2D: 1 array of [5]int
// Should count 1 (not 5)
// ==============================
int a25[][5] = { 10, 20, 30, 40, 50 };

// ==============================
// 26. Flexible 2D: 4 arrays of [2]int with mixed styles
// Some braced, some not
// ==============================
int a26[][2] = {
    { 1, 2 },
    3, 4,
    { 5, 6 },
    7, 8
};

// ==============================
// 27. Flexible 3D: 2 arrays of [2][2]int with partial explicit
// Should count 2
// ==============================
int a27[][2][2] = {
    { { 1, 2 }, 3, 4 },
    5, 6, 7, 8
};

// ==============================
// 28. Partial initialization test (should fill rest with zeros)
// Fixed array, only initialize first 2 of 5 elements
// ==============================
int a28[5] = { 10, 20 };

// ==============================
// 29. Partial initialization of 2D array (flexible outer)
// Initialize 1 complete array of [3]int with 3 elements
// ==============================
int a29[][3] = { 100, 200, 300 };

// ==============================
// 30. Huge array, should parse without running out of memory!
// ==============================
int a30[4294967296] = { 1, 2, 3, };

// ==============================
// 13. Excess initialisers (should parse but warn/error)
// ==============================
//int a13[2] = { 1, 2, 3 };

