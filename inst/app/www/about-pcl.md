Six synthetic DNA control targets are included with every nCounter Gene Expression assay.  
Their concentrations range linearly from `128 fM` to `0.125 fM`, and they are referred to as **POS_A** to **POS_F**, respectively.  
These **Positive Controls** are typically used to measure the efficiency of the hybridization reaction, and their step-wise concentrations also make them useful in checking the linearity performance of the assay.  
An R2 value is calculated from the regression between the known concentration of each of the **Positive Controls** and the resulting counts from them (this calculation is performed using log2-transformed values).

Since the known concentrations of the **Positive Controls** increase in a linear fashion, the resulting counts should, as well.  
Therefore, R2 values should be higher than `0.95`.

Note that because POS_F has a known concentration of `0.125 fM`, which is considered below the limit of detection of the system, it should be excluded from this calculation (although you will see that **POS_F** counts are significantly higher than the negative control counts in most cases).