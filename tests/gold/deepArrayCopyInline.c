#include "deepArrayCopyInline.h"


void deepArrayCopyInline(struct awl_awl_awl_unsignedS32 * v0, struct s_2_awl_awl_awl_unsignedS32_awl_awl_awl_unsignedS32 * out)
{
  {
    struct awl_awl_unsignedS32 * dst_i9 = ((*out).member1).buffer;
    int32_t dstLen_i9 = ((*out).member1).length;
    struct awl_awl_unsignedS32 * src_i9 = (*v0).buffer;
    int32_t srcLen_i9 = (*v0).length;
    
    {
      struct awl_awl_unsignedS32 * dst_i7_i9 = dst_i9;
      uint32_t oldLen_i7_i9 = dstLen_i9;
      uint32_t newLen_i7_i9 = srcLen_i9;
      
      if ((oldLen_i7_i9 /= newLen_i7_i9))
      {
        if ((oldLen_i7_i9 < newLen_i7_i9))
        {
          dst_i7_i9 = resizeArray(dst_i7_i9, sizeof(struct awl_awl_unsignedS32), newLen_i7_i9);
          for (int32_t i_i7_i9 = oldLen_i7_i9;
               i_i7_i9 < newLen_i7_i9;
               i_i7_i9 += 1)
          {
            struct awl_awl_unsignedS32 null_arr_0_i7_i9 = { 0 };
            
            dst_i7_i9[i_i7_i9] = null_arr_0_i7_i9;
          }
        }
        else
        {
          for (int32_t i_i7_i9 = newLen_i7_i9;
               i_i7_i9 < oldLen_i7_i9;
               i_i7_i9 += 1)
          {
            {
              struct awl_unsignedS32 * src_i0_i7_i9 = (dst_i7_i9[i_i7_i9]).buffer;
              int32_t srcLen_i0_i7_i9 = (dst_i7_i9[i_i7_i9]).length;
              
              for (int32_t i_i0_i7_i9 = 0;
                   i_i0_i7_i9 < srcLen_i0_i7_i9;
                   i_i0_i7_i9 += 1)
              {
                freeArray((src_i0_i7_i9[i_i0_i7_i9]).buffer);
              }
              freeArray(src_i0_i7_i9);
            }
          }
          dst_i7_i9 = resizeArray(dst_i7_i9, sizeof(struct awl_awl_unsignedS32), newLen_i7_i9);
        }
      }
      dst_i9 = dst_i7_i9;
    }
    dstLen_i9 = srcLen_i9;
    {
      struct awl_awl_unsignedS32 * dst_i8_i9 = dst_i9;
      int32_t dstLen_i8_i9 = dstLen_i9;
      struct awl_awl_unsignedS32 * src_i8_i9 = src_i9;
      int32_t srcLen_i8_i9 = srcLen_i9;
      int32_t pos_i8_i9 = 0;
      
      for (int32_t i_i8_i9 = 0; i_i8_i9 < srcLen_i8_i9; i_i8_i9 += 1)
      {
        {
          struct awl_unsignedS32 * dst_i5_i8_i9 = (dst_i8_i9[(pos_i8_i9 + i_i8_i9)]).buffer;
          int32_t dstLen_i5_i8_i9 = (dst_i8_i9[(pos_i8_i9 + i_i8_i9)]).length;
          struct awl_unsignedS32 * src_i5_i8_i9 = (src_i8_i9[i_i8_i9]).buffer;
          int32_t srcLen_i5_i8_i9 = (src_i8_i9[i_i8_i9]).length;
          
          {
            struct awl_unsignedS32 * dst_i3_i5_i8_i9 = dst_i5_i8_i9;
            uint32_t oldLen_i3_i5_i8_i9 = dstLen_i5_i8_i9;
            uint32_t newLen_i3_i5_i8_i9 = srcLen_i5_i8_i9;
            
            if ((oldLen_i3_i5_i8_i9 /= newLen_i3_i5_i8_i9))
            {
              if ((oldLen_i3_i5_i8_i9 < newLen_i3_i5_i8_i9))
              {
                dst_i3_i5_i8_i9 = resizeArray(dst_i3_i5_i8_i9, sizeof(struct awl_unsignedS32), newLen_i3_i5_i8_i9);
                for (int32_t i_i3_i5_i8_i9 = oldLen_i3_i5_i8_i9;
                     i_i3_i5_i8_i9 < newLen_i3_i5_i8_i9;
                     i_i3_i5_i8_i9 += 1)
                {
                  struct awl_unsignedS32 null_arr_0_i3_i5_i8_i9 = { 0 };
                  
                  dst_i3_i5_i8_i9[i_i3_i5_i8_i9] = null_arr_0_i3_i5_i8_i9;
                }
              }
              else
              {
                for (int32_t i_i3_i5_i8_i9 = newLen_i3_i5_i8_i9;
                     i_i3_i5_i8_i9 < oldLen_i3_i5_i8_i9;
                     i_i3_i5_i8_i9 += 1)
                {
                  freeArray((dst_i3_i5_i8_i9[i_i3_i5_i8_i9]).buffer);
                }
                dst_i3_i5_i8_i9 = resizeArray(dst_i3_i5_i8_i9, sizeof(struct awl_unsignedS32), newLen_i3_i5_i8_i9);
              }
            }
            dst_i5_i8_i9 = dst_i3_i5_i8_i9;
          }
          dstLen_i5_i8_i9 = srcLen_i5_i8_i9;
          {
            struct awl_unsignedS32 * dst_i4_i5_i8_i9 = dst_i5_i8_i9;
            int32_t dstLen_i4_i5_i8_i9 = dstLen_i5_i8_i9;
            struct awl_unsignedS32 * src_i4_i5_i8_i9 = src_i5_i8_i9;
            int32_t srcLen_i4_i5_i8_i9 = srcLen_i5_i8_i9;
            int32_t pos_i4_i5_i8_i9 = 0;
            
            for (int32_t i_i4_i5_i8_i9 = 0;
                 i_i4_i5_i8_i9 < srcLen_i4_i5_i8_i9;
                 i_i4_i5_i8_i9 += 1)
            {
              (dst_i4_i5_i8_i9[(pos_i4_i5_i8_i9 + i_i4_i5_i8_i9)]).buffer = initCopyArray((dst_i4_i5_i8_i9[(pos_i4_i5_i8_i9 + i_i4_i5_i8_i9)]).buffer, (dst_i4_i5_i8_i9[(pos_i4_i5_i8_i9 + i_i4_i5_i8_i9)]).length, sizeof(uint32_t), (src_i4_i5_i8_i9[i_i4_i5_i8_i9]).buffer, (src_i4_i5_i8_i9[i_i4_i5_i8_i9]).length);
              (dst_i4_i5_i8_i9[(pos_i4_i5_i8_i9 + i_i4_i5_i8_i9)]).length = (src_i4_i5_i8_i9[i_i4_i5_i8_i9]).length;
            }
            dst_i5_i8_i9 = dst_i4_i5_i8_i9;
          }
          (dst_i8_i9[(pos_i8_i9 + i_i8_i9)]).buffer = dst_i5_i8_i9;
        }
        (dst_i8_i9[(pos_i8_i9 + i_i8_i9)]).length = (src_i8_i9[i_i8_i9]).length;
      }
      dst_i9 = dst_i8_i9;
    }
    ((*out).member1).buffer = dst_i9;
  }
  ((*out).member1).length = (*v0).length;
  {
    struct awl_awl_unsignedS32 * dst_i10 = ((*out).member2).buffer;
    int32_t dstLen_i10 = ((*out).member2).length;
    struct awl_awl_unsignedS32 * src_i10 = (*v0).buffer;
    int32_t srcLen_i10 = (*v0).length;
    
    {
      struct awl_awl_unsignedS32 * dst_i7_i10 = dst_i10;
      uint32_t oldLen_i7_i10 = dstLen_i10;
      uint32_t newLen_i7_i10 = srcLen_i10;
      
      if ((oldLen_i7_i10 /= newLen_i7_i10))
      {
        if ((oldLen_i7_i10 < newLen_i7_i10))
        {
          dst_i7_i10 = resizeArray(dst_i7_i10, sizeof(struct awl_awl_unsignedS32), newLen_i7_i10);
          for (int32_t i_i7_i10 = oldLen_i7_i10;
               i_i7_i10 < newLen_i7_i10;
               i_i7_i10 += 1)
          {
            struct awl_awl_unsignedS32 null_arr_0_i7_i10 = { 0 };
            
            dst_i7_i10[i_i7_i10] = null_arr_0_i7_i10;
          }
        }
        else
        {
          for (int32_t i_i7_i10 = newLen_i7_i10;
               i_i7_i10 < oldLen_i7_i10;
               i_i7_i10 += 1)
          {
            {
              struct awl_unsignedS32 * src_i0_i7_i10 = (dst_i7_i10[i_i7_i10]).buffer;
              int32_t srcLen_i0_i7_i10 = (dst_i7_i10[i_i7_i10]).length;
              
              for (int32_t i_i0_i7_i10 = 0;
                   i_i0_i7_i10 < srcLen_i0_i7_i10;
                   i_i0_i7_i10 += 1)
              {
                freeArray((src_i0_i7_i10[i_i0_i7_i10]).buffer);
              }
              freeArray(src_i0_i7_i10);
            }
          }
          dst_i7_i10 = resizeArray(dst_i7_i10, sizeof(struct awl_awl_unsignedS32), newLen_i7_i10);
        }
      }
      dst_i10 = dst_i7_i10;
    }
    dstLen_i10 = srcLen_i10;
    {
      struct awl_awl_unsignedS32 * dst_i8_i10 = dst_i10;
      int32_t dstLen_i8_i10 = dstLen_i10;
      struct awl_awl_unsignedS32 * src_i8_i10 = src_i10;
      int32_t srcLen_i8_i10 = srcLen_i10;
      int32_t pos_i8_i10 = 0;
      
      for (int32_t i_i8_i10 = 0; i_i8_i10 < srcLen_i8_i10; i_i8_i10 += 1)
      {
        {
          struct awl_unsignedS32 * dst_i5_i8_i10 = (dst_i8_i10[(pos_i8_i10 + i_i8_i10)]).buffer;
          int32_t dstLen_i5_i8_i10 = (dst_i8_i10[(pos_i8_i10 + i_i8_i10)]).length;
          struct awl_unsignedS32 * src_i5_i8_i10 = (src_i8_i10[i_i8_i10]).buffer;
          int32_t srcLen_i5_i8_i10 = (src_i8_i10[i_i8_i10]).length;
          
          {
            struct awl_unsignedS32 * dst_i3_i5_i8_i10 = dst_i5_i8_i10;
            uint32_t oldLen_i3_i5_i8_i10 = dstLen_i5_i8_i10;
            uint32_t newLen_i3_i5_i8_i10 = srcLen_i5_i8_i10;
            
            if ((oldLen_i3_i5_i8_i10 /= newLen_i3_i5_i8_i10))
            {
              if ((oldLen_i3_i5_i8_i10 < newLen_i3_i5_i8_i10))
              {
                dst_i3_i5_i8_i10 = resizeArray(dst_i3_i5_i8_i10, sizeof(struct awl_unsignedS32), newLen_i3_i5_i8_i10);
                for (int32_t i_i3_i5_i8_i10 = oldLen_i3_i5_i8_i10;
                     i_i3_i5_i8_i10 < newLen_i3_i5_i8_i10;
                     i_i3_i5_i8_i10 += 1)
                {
                  struct awl_unsignedS32 null_arr_0_i3_i5_i8_i10 = { 0 };
                  
                  dst_i3_i5_i8_i10[i_i3_i5_i8_i10] = null_arr_0_i3_i5_i8_i10;
                }
              }
              else
              {
                for (int32_t i_i3_i5_i8_i10 = newLen_i3_i5_i8_i10;
                     i_i3_i5_i8_i10 < oldLen_i3_i5_i8_i10;
                     i_i3_i5_i8_i10 += 1)
                {
                  freeArray((dst_i3_i5_i8_i10[i_i3_i5_i8_i10]).buffer);
                }
                dst_i3_i5_i8_i10 = resizeArray(dst_i3_i5_i8_i10, sizeof(struct awl_unsignedS32), newLen_i3_i5_i8_i10);
              }
            }
            dst_i5_i8_i10 = dst_i3_i5_i8_i10;
          }
          dstLen_i5_i8_i10 = srcLen_i5_i8_i10;
          {
            struct awl_unsignedS32 * dst_i4_i5_i8_i10 = dst_i5_i8_i10;
            int32_t dstLen_i4_i5_i8_i10 = dstLen_i5_i8_i10;
            struct awl_unsignedS32 * src_i4_i5_i8_i10 = src_i5_i8_i10;
            int32_t srcLen_i4_i5_i8_i10 = srcLen_i5_i8_i10;
            int32_t pos_i4_i5_i8_i10 = 0;
            
            for (int32_t i_i4_i5_i8_i10 = 0;
                 i_i4_i5_i8_i10 < srcLen_i4_i5_i8_i10;
                 i_i4_i5_i8_i10 += 1)
            {
              (dst_i4_i5_i8_i10[(pos_i4_i5_i8_i10 + i_i4_i5_i8_i10)]).buffer = initCopyArray((dst_i4_i5_i8_i10[(pos_i4_i5_i8_i10 + i_i4_i5_i8_i10)]).buffer, (dst_i4_i5_i8_i10[(pos_i4_i5_i8_i10 + i_i4_i5_i8_i10)]).length, sizeof(uint32_t), (src_i4_i5_i8_i10[i_i4_i5_i8_i10]).buffer, (src_i4_i5_i8_i10[i_i4_i5_i8_i10]).length);
              (dst_i4_i5_i8_i10[(pos_i4_i5_i8_i10 + i_i4_i5_i8_i10)]).length = (src_i4_i5_i8_i10[i_i4_i5_i8_i10]).length;
            }
            dst_i5_i8_i10 = dst_i4_i5_i8_i10;
          }
          (dst_i8_i10[(pos_i8_i10 + i_i8_i10)]).buffer = dst_i5_i8_i10;
        }
        (dst_i8_i10[(pos_i8_i10 + i_i8_i10)]).length = (src_i8_i10[i_i8_i10]).length;
      }
      dst_i10 = dst_i8_i10;
    }
    ((*out).member2).buffer = dst_i10;
  }
  ((*out).member2).length = (*v0).length;
}
