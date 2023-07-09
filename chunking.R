
# input:  head vector, type ('anderson' or 'macutek')
# output: chunk memberships


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
      if (position == heads[position+1] | heads[position] == position+1 | heads[position] == heads[position+1]) {  # $$ yes
        # check the surroundings
        if (any(position == heads[position+1] & any(positions[chunk_names==chunk_names[position]]==heads[position]),  # 
                heads[position] == position+1 & any(heads[chunk_names==chunk_names[position]]==position),  # 
                heads[position] == heads[position+1] & 
                # have we already seen the common head?
                if (heads[position] < position) {   # ++ yes
                  chunk_names[heads[position]] != chunk_names[position]  # is it in a different chunk?
                } else {   # ++ no
                  any(heads[chunk_names==chunk_names[position]]==position) |  # is the word already in a dependency?
                    any(sapply(position:(heads[position]-1), function(j) heads[j]!=heads[position])) # is there any non sibling word between them and their head?
                })) {
          # %% yes: assign new chunk
          chunk_names[position+1] <-  chunk_names[position] + 1
        } else {
          # %% no: assign to same chunk
          chunk_names[position+1] = chunk_names[position]
        }
      } else {
        # $$ no: assign to new chunk
        chunk_names[position+1] <-  chunk_names[position] + 1
      }
    }
  } else { 
    print("invalid chunking type, must be in ('macutek','anderson')")
    chunk_names = NULL}
  
  return(chunk_names)
}


# example
head_vector <- c(2,5,2,5,0,9,9,9,10,5)
head_vector <- c(2,3,0)
assign_chunks(head_vector,'macutek')
