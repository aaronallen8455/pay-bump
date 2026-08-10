Sound semantics for oneOf:
 ❯ Is there a correct way to merge the oneOf properties of the two schemas if there is no discriminator, i.e. no way to correlate between base and extension? Merging them pairwise seems too arbitrary.
                                                                                                                                                                                                                              
 ⌄ Thought for 12s                                                                                                                                                                                                            
 │ Good question about schema merging semantics.                                                                                                                                                                              
                                                                                                                                                                                                                              
 ● This is a fundamental semantic question worth thinking through carefully.                                                                                                                                                  
                                                                                                                                                                                                                              
   The Core Problem                                                                                                                                                                                                           
                                                                                                                                                                                                                              
    oneOf  means "exactly one of these sub-schemas must match." When merging two schemas that each have  oneOf , you're essentially asking: what does the merged type's  oneOf  mean?                                         
                                                                                                                                                                                                                              
   If the base has  oneOf: [A, B]  and the extension has  oneOf: [X, Y] , a value must satisfy both constraints simultaneously — it must match exactly one of  {A, B}  AND exactly one of  {X, Y} . The merged schema needs to
   express all valid combinations.                                                                                                                                                                                            
                                                                                                                                                                                                                              
   The Correct Semantics                                                                                                                                                                                                      
                                                                                                                                                                                                                              
   The mathematically correct result is the Cartesian product of the two  oneOf  lists — each pair  (A,X) ,  (A,Y) ,  (B,X) ,  (B,Y)  merged together:                                                                        
                                                                                                                                                                                                                              
   oneOf: [merge(A,X), merge(A,Y), merge(B,X), merge(B,Y)]                                                                                                                                                                    
                                                                                                                                                                                                                              
   This is sound because:                                                                                                                                                                                                     
                                                                                                                                                                                                                              
   • A valid value satisfies exactly one base variant and exactly one extension variant                                                                                                                                       
   • Each merged pair represents one valid combination                                                                                                                                                                        
   • The  oneOf  at the top level still enforces "exactly one"                                                                                                                                                                
                                                                                                                                                                                                                              
   Caveats                                                                                                                                                                                                                    
                                                                                                                                                                                                                              
   1. Combinatorial explosion. With  m  base variants and  n  extension variants you get  m×n  merged schemas. In practice this may be acceptable since discriminated unions tend to be small.                                
                                                                                                                                                                                                                              
   2. Non-disjointness. If merged pairs aren't mutually exclusive,  oneOf  validation could spuriously reject valid values (since it requires exactly one match). This is a pre-existing problem though — it's not introduced 
   by the merge strategy.                                                                                                                                                                                                     
                                                                                                                                                                                                                              
   3. Discriminator. The discriminator mapping would need to be rebuilt. Without knowing which pairs share a discriminator key, the discriminator may have to be dropped from the merged schema, which degrades tooling       
   support.                                                                                                                                                                                                                   
                                                                                                                                                                                                                              
   What the Current Code Should Do                                                                                                                                                                                            
                                                                                                                                                                                                                              
   • With discriminator: current approach (1-to-1 pairing by key) is the right idea, but it drops unmatched base variants. Unmatched pairs could fall back to Cartesian product, or be preserved as-is.                       
   • Without discriminator: Cartesian product of pairwise merges is the only semantically defensible choice. The current fallback (use extension's  oneOf  verbatim) is incorrect.                                            
                                                                                                                                                                                                                              
   So: yes, there is a correct approach even without a discriminator — it's just more expensive to represent in the schema.                                                                                                   

Consideration of allOf, anyOf, not?

- Optional extension could be possible by creating clones of referenced schemas
  that have all fields as optional and then replacing references to existing schemas
  with the optional version in the optional extension. Is it really useful though?
