#infix operator
'+'(3,6)

 `-`(`*`(5,3),1)

#eval function
eval(3%%4)   #eval evalutes all the operators

eval(sin(pi/2))
  
#calculator                  no neec for exception handling x/0=inf
calc=function(x,opr,y)
{
  z=paste(x,opr,y,sep='')     #returning expression in string type without space
  res=eval(parse(text=z))     #text is parameter of parse() which convert string into expression 
  print(res)
}

ch=readline("ENTER CHOICE : +,-,*,/ : ")

f_num=as.integer(readline("ENTER FIRST NUMBER"))
s_num=as.integer(readline("ENTER SECOND NUMBER"))
calc(f_num,ch,s_num)


