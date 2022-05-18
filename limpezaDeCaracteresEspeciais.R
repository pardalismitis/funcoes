limpezaGeral <- function(x){
  #caracteres a serem substituidos
  AccChars <- "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿ "
  #caracteres para substituir
  RegChars <- "SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyy_"
  temp = x
  #procurar em todas as colunas
  
  for(c in 1:ncol(x)){
    for(l in 1:nrow(x)){ 
      #mas apenas as colunas de texto (char) =)
      
      if(is.character(x[l, c])){
        #trocar todos os diacriticos
        
        temp[l, c] = chartr(AccChars, RegChars, x[l, c])
        #passar tudo para minuscula
        
        temp[l, c] = tolower(temp[l, c])
      }
    }
  }
  return(temp)
}
