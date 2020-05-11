#PRIME NO.
num = as.integer(readline(prompt="Enter a number : "))
for(i in 2:num-1) 
{
  if (num %% i == 0)
  {
    flag = F
    break()
    }
  else 
    {
    flag = T
    }
}
if(flag==T) 
  {
  print("Prime Number")
} else 
{
  print("Not Prime Number")
}

