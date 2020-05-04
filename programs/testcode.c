int a;

int addThem(int d, int e) {
  int f;
  f = d + e;

  return f;
}

void putDigit(int s) {
   putchar(48+s);
}

void printInt(int r) {
  int t;
  int found;

  found = 0;

  if (r >= 10000) {
      /* print -1) */
    putchar(45);
    putDigit(1);
    return;
  }
  else {
    if (r >= 1000) {
       t = r / 1000;
       putDigit(t);
       r = r - t * 1000;
       found=1;
    }

    if (r >= 100) {
       t = r / 100;
       putDigit(t);
       r = r - t * 100;
       found=1;
    }
    else if (found == 1) {
       putDigit(0);
    }

    if (r >= 10) {
       t = r / 10;
       putDigit(t);
       r = r - t * 10;
    }
    else if (found == 1) {
       putDigit(0);
    }

    putDigit(r);
  }

}

int main (void) {

  int b;
  int c;
  int g;
  int h;
  int i;

  b = c = 5;

  if (b == 5) {
    a = 3;
  }
  else {
    a = 4;
  }

  g = 0;
  i = 1;
  while (i <= 8) {
    g = g + i;
    i = i+1;
  }
  h = g / 3;
  g = h * 4;

  c = addThem(a, b);
  putchar (56);
  putchar (61);
  putchar (c+g);
  putchar (10);

  i = 0;
  while (i < 10) {
    putchar(48+i);
    i = i+1;
  }
  putchar(10);
  putchar(67);
  putchar(83); 
  printInt(3510);
  putchar(10);

  b = 0;
  c = 1;
  g = 1;
  h = 0;
  i = 0;

  if (b == 0) {
    if (c==0) {
      i = 1;
    }
    else if (g == 0) {
      i = 2;
    }
    else if (h == 0) {
      i = 10;
    }
    else {
      i = 3;
    }
  }
  else {
    i = 0;
  }


  if (i == 10) {
    putchar(99);
    putDigit(0);
    putDigit(0); 
    putchar(108);  
  }
  else {
    putchar(98);
    putchar(97);
    putchar(100);
    putchar(61);
    printInt(i);
  }
  putchar(10);

  return 0;
}

