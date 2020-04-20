## ----global_options, include = FALSE-----------------------------------------------------------------
try(source("../../.Rprofile"))


## ----------------------------------------------------------------------------------------------------
# Sort and generate variable equal to sorted index
df_iris <- iris %>% arrange(Sepal.Length) %>%
              mutate(Sepal.Len.Index = row_number()) %>%
              select(Sepal.Length, Sepal.Len.Index, everything())

# Show results Head 10
df_iris %>% head(10) %>%
  kable() %>%
  kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------
# 1. Sort
df_iris_m1 <- iris %>% mutate(Sepal.Len.Lowest.all = min(Sepal.Length)) %>%
                select(Sepal.Length, Sepal.Len.Lowest.all, everything())


# Show results Head 10
df_iris_m1 %>% head(10) %>%
  kable() %>%
  kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------
# 1. Sort
# 2. generate index
# 3. value at lowest index (case_when)
# 4. spread value from lowest index to other rows
# Note step 4 does not require step 3
df_iris_m2 <- iris %>% arrange(Sepal.Length) %>%
              mutate(Sepal.Len.Index = row_number()) %>%
              mutate(Sepal.Len.Lowest.one =
                       case_when(row_number()==1 ~ Sepal.Length)) %>%
              mutate(Sepal.Len.Lowest.all =
                       Sepal.Length[Sepal.Len.Index==1]) %>%
              select(Sepal.Length, Sepal.Len.Index,
                     Sepal.Len.Lowest.one, Sepal.Len.Lowest.all)


# Show results Head 10
df_iris_m2 %>% head(10) %>%
  kable() %>%
  kable_styling_fc_wide()


## ----------------------------------------------------------------------------------------------------
# 1. Sort and generate variable equal to sorted index
# 2. Plus or minus deviations from some value
# 3. Find the zero, which means, the number closests to zero including zero from the negative side
# 4. Find the index at the highest zero and below deviation point
# 5. Difference of zero index and original sorted index
sc_val_x <- 4.65
df_iris_deviate <- iris %>% arrange(Sepal.Length) %>%
              mutate(Sepal.Len.Index = row_number()) %>%
              mutate(Sepal.Len.Devi = (Sepal.Length - sc_val_x)) %>%
              mutate(Sepal.Len.Devi.Neg =
                       case_when(Sepal.Len.Devi <= 0 ~ (-1)*(Sepal.Len.Devi))) %>%
              arrange((Sepal.Len.Devi.Neg), desc(Sepal.Len.Index)) %>%
              mutate(Sepal.Len.Index.Zero =
                       case_when(row_number() == 1 ~ Sepal.Len.Index)) %>%
              mutate(Sepal.Len.Devi.Index =
                       Sepal.Len.Index - Sepal.Len.Index.Zero[row_number() == 1]) %>%
              arrange(Sepal.Len.Index) %>%
              select(Sepal.Length, Sepal.Len.Index, Sepal.Len.Devi,
                     Sepal.Len.Devi.Neg, Sepal.Len.Index.Zero, Sepal.Len.Devi.Index)


# Show results Head 10
df_iris_deviate %>% head(20) %>%
  kable() %>%
  kable_styling_fc_wide()

