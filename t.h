uint64_t bin_long_c (const int64_t* input, uint64_t len, int64_t key);

//macro to generate binary search function for any basic C type
#define BIN_INIT(name, input_t, len_t) \
len_t bin_##name##_c (const input_t* input, len_t len, input_t key){ \
  len_t start=0, end=len; \
  len_t middle; \
  while (start < end){ \
      middle = (start + end)/2; \
      if(key > input[middle]) \
        start = middle + 1; \
      else end = middle; \
  } \
  return start; \
}
