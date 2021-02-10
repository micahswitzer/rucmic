// declare external linkage
int putchar(int c);

int main(int argv, int argc) {
    // variable declarations can't have intializers
    int a;
    int h;
    int i;
    int lf;

    // ASCII 'a' = 97
    a = 97;
    // h = 7th letter
    h = a + 7;
    // i = 8th letter
    i = a + 8;
    // ASCII line feed = 10
    lf = 10;

    // say "hi" :)
    putchar(h);
    putchar(i);
    putchar(lf);

    return 0;
}
