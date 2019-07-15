library(jsonlite)

pi

jo = toJSON(pi, digits = 3)
fromJSON(jo)


ci = 'da'
joci = toJSON(ci)
fromJSON(joci)

su = c('a','s','d')
josu = toJSON(su)
fromJSON(josu)


#[
#  {
#    "name": "test",
#    "age": 25,
#    "sex":"M",
#    "address":"seoul",
#    "hobby":"basketball"
#    
#  }
#]

name = c("test")
age = c(25)
sex = c("m")
address = c("seoul")
hobby = c("basketball")
person = data.frame(name,age,sex,address,hobby)
jp = toJSON(person)
prettify(jp)

data=fromJSON("C:/Users/709-000/Downloads/03_JSON/person.json")
data
class(data)

data1=fromJSON("C:/Users/709-000/Downloads/03_JSON/sample.json")

data1 = as.data.frame(data1)
names(data1) = c("a","s","d","f","g","h","j")
data1$s = as.numeric(as.character(data1$s))

#csv로 저장
write.csv(data1, 'data1.csv')




df_j = fromJSON("https://api.github.com/users/hadley/repos")
str(df_j)
names(df_j)
names(df_j$owner)

#converting R dataframe to json
jr =toJSON(df_j)
cat(jr)
minify(jr)
prettify(jr)

js = toJSON(df_js)