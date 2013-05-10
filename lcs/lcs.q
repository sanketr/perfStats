// Algorithm B from Hirschberg's linear Space Longest Common Subsequence Algorithm
lcslength:{[a;b;f]
  m: count a; n: count b;
  //Step 1 - equivalent of initializing K(1,J)
  k1: (n+1)#0;
  i: -1;
  //Step 2 - nested loops on i and j
  while[m > i+:1; //iterate on 0,1,2,...m-1
      k0: k1; //set K0 to K1
      //0N!k0;
      j:-1;
      while[n > j+:1; //iterate on 0,1,2...n-1
        //0N!j;
        //0N!k0; 0N!k1;
        $[f[a[i];b[j]];  //check if match
          k1[j+1]: k0[j] + 1;
          k1[j+1]: max (k1[j];k0[j+1])];
        ];
    ];
  :k1
  };

//Algorithm C from Hirschberg's Linear Space Longest Common Subsequence Algorithm 
// - returns the common subsequence
lcs:{[a;b;f]
  m: count a;
  $[m=0;:();m=1;$[first a in b;:a[0];:()];];
  i:floor m%2;
  ab: i#a; ae: i _ a; //beginning a; ending a
  llb: lcslength[ab;b;f];
  lle: lcslength[reverse ae;reverse b;f];
  k: m?max m:llb + reverse lle; /step 4 of algorithm C - find the min column to split
  //now split b along k, and call lcs recursively - O(log (count b)) recursions - not many
  :raze (lcs[ab;k#b;f];lcs[ae;k _ b;f])
  }

//Algorithm C from Hirschberg's Linear Space Longest Common Subsequence Algorithm - 
//index version - returns indices in a and b that form the longest common subsequence
lcsi:{[a;b;f]
  m: count a[0];
  $[m=0;:();m=1;$[count idx:where b[0] in a[0];:enlist (first a[1];b[1] first idx);:()];];
  i:floor m%2;
  ab: (i#a[0];i#a[1]); ae: (i _ a[0];i _ a[1]); //beginning a; ending a
  llb: lcslength[ab[0];b[0];f];
  lle: lcslength[reverse ae[0];reverse b[0];f];
  k: m?max m:llb + reverse lle; /step 4 of algorithm C - find the min column to split
  //now split b along k, and call lcs recursively - O(log (count b)) recursions - not many
  :raze (lcsi[ab;(k#b[0];k#b[1]);f];lcsi[ae;(k _ b[0]; k _ b[1]);f])
  }

diffTables:{[t1;t2;s;c]
  i1: exec i from t1 where sym in s;
  i2: exec i from t2 where sym in s;
  a:flip (t1 i1) c;
  b:flip (t2 i2) c;
  c:lcsi[(a;til count a);(b;til count b);{[x;y] all x=y}];
  dela: (til count a) except c[;0]; //return delta indices, i.e., not a common subsequence in a
  delb: (til count b) except c[;1]; //return delta indices, i.e., not a common subsequence in b
  :(i1 dela; i2 delb) //return the delta indices in original table
 }
