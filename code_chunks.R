
# chunks as vertices
assign_chunks <- function(heads,type) {
  n <- length(heads)
  chunk_names <- 1:n
  positions <- 1:n
  if (type=="macutek") {
    # are linear neighbors also dependency neighbors ?
    for (position in 2:n) { 
      # is word in same chunk as previous word?
      if (heads[position] == position-1 | position == heads[position-1]) {
        # - yes: assign same to chunk
        chunk_names[position] = chunk_names[position-1]
      } else {
        # - no: assign to following chunk
        chunk_names[position] <- chunk_names[position-1] + 1
      }
    }
  } else if (type=="anderson") {
    for (position in 1:(n-1)) { 
      # is word in lin-dep relation with previous word, or they share the head?
      if (position == heads[position+1] | heads[position] == position+1 | heads[position] == heads[position+1]) {
        # check the surroundings
        if (any(position == heads[position+1] & any(positions[chunk_names==chunk_names[position]]==heads[position]),  # 
                heads[position] == position+1 & any(heads[chunk_names==chunk_names[position]]==position),  # 
                heads[position] == heads[position+1] & 
                # have we already seen the common head?
                if (heads[position] < position) {   
                  chunk_names[heads[position]] != chunk_names[position]  # is it in a different chunk?
                } else {
                  any(heads[chunk_names==chunk_names[position]]==position) |  # is the word already in a dependency?
                    any(sapply(position:(heads[position]-1), function(j) heads[j]!=heads[position])) #is there any non sibling word between them and their head?
                })) {
          # yes: assign new chunk
          chunk_names[position+1] <-  chunk_names[position] + 1
        } else {
          # no: assign to same chunk
          chunk_names[position+1] = chunk_names[position]
        }
      } else {
        # - no: assign to new chunk
        chunk_names[position+1] <-  chunk_names[position] + 1
      }
    }
  } else { 
    print("invalid chunking type, must be in 'macutek', 'anderson'")
    chunk_names = NULL}
  
  return(chunk_names)
}


## read one language
ReadOneLangChunks <- function(collection,ISO_language,type="macutek"){
  forrest <- readForrest(collection,ISO_language)
  rows <- lapply(1:length(forrest), function(i) {
    heads <- forrest[[i]]
    sentence_ID <- i
    n <- length(heads)
    positions <- 1:n
    # get partition in segments
    chunk_names <- assign_chunks(heads,type)
    # get values of d
    d <- sapply(positions, function(j){
      chunk1 <- chunk_names[j]
      chunk2 <- ifelse(heads[j]!=0,chunk_names[positions==heads[j]],chunk1)
      abs(chunk1-chunk2)
    })
    d <- d[d!=0]
    new_n <- length(unique(chunk_names))
    if (length(d)>=1) data.frame("ISO_language"=ISO_language,"sentence_ID"=sentence_ID,
                                 "word_num"=n,"sent_n"=new_n,"d"= d)
  })
  do.call(rbind,rows)
}


### read a whole collection, get df of dd
ReadColl <- function(collection,chunks=F) {
  trees <- if (chunks==F) { 
    lapply(ISO,ReadOneLang,collection=collection)
  } else lapply(ISO,ReadOneLangChunks,collection=collection,type=chunks)
  forrest <- do.call(rbind,trees)
  return(forrest)
}

head_from_positions <- function(new_pos,old_heads) {
  sapply(new_pos, function(x) {
    old_head = old_heads[x]
    new_head = ifelse(old_head==0,0,which(new_pos==old_head))
    new_head
  })
}

## write one language in renormalized structure
WriteNormalizedHeads <- function(collection,ISO_language,type="anderson"){
  forrest <- readForrest(collection,ISO_language)
  rows <- lapply(1:length(forrest), function(i) {
    heads <- forrest[[i]]
    sentence_ID <- i
    n <- length(heads)
    positions <- 1:n
    # get partition in segments
    chunk_names <- assign_chunks(heads,type)
    groups <- unique(chunk_names)
    # compute chunk level sigma (and save new structures)
    res <- lapply(groups, function(group) {
      chunk_heads <- heads[chunk_names==group]
      chunk_pos <- positions[chunk_names==group]
      which_head <- chunk_heads[which(chunk_heads %!in% chunk_pos)]
      if (which_head==0) 0 else chunk_names[which_head]
    })
    # new heads to write
    do.call(paste,res)
  })
  # write renormalized structures
  fileConn <- file(paste("collections/",tolower(collection),"_",type,"/",LANGS[ISO==ISO_language],".heads",sep=""))
  writeLines(do.call(c,rows), fileConn)
  close(fileConn)
}

