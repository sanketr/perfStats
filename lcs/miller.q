//Implementation of "An O(NP) Sequence Comparison Algorithm" 
//by Sun Wu, Udi Manber, Gene Myers and Web Miller
//Note: P=(D-(M-N))/2, D is shortest edit distance, M>=N

ses:{[a;b;f]
      //TODO: swap if n < m
      n:count b;m:count a;
      delta:n-m;
      offset:m+1;
      fp:(m+n+3)#(-1);p:-1;
      pdict:()!();
      while[fp[delta+offset] < n;
        p+:1;
        kmin: neg p; kmax: delta-1;k:kmin-1;
        while[kmax >= k+:1;fp[k+offset]:diags[a;b;m;n;k;max(fp[k+offset-1]+1;fp[k+offset+1])]];
        kmax: delta + p; kmin: delta + 1; k:kmax+1;
        while[kmin <= k-:1;fp[k+offset]:diags[a;b;m;n;k;max(fp[k+offset-1]+1;fp[k+offset+1])]];
        fp[delta+offset]: diags[a;b;m;n;k;max(fp[delta+offset-1]+1;fp[delta+offset+1])];
        pdict,:(enlist p)!enlist fp;
       ];
    :pdict;
  }

   

diags:{[a;b;m;n;k;y]
    x:y-k; while[(x<m) and (y<n) and a[x]=b[y]; x+:1; y+:1];y}
