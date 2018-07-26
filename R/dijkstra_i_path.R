dijkstra_i_path = function(weight_mat, source_node, many_path = FALSE) {
  ## the dijkstra method to calculate the min distance from any nodes in the  graph to the given source_node
  ##  input:
  ##      weight_mat: the weight matrix of the graph N*N
  ##      source_node: the given source node 1,2,3......N
  ##      many_path: if output all the shortest path if any
  ##  output:
  ## result: the min distance from any nodes to the source
  ## path: if many_path, a list of N elements, each is all the shortest path,
  ##              or is a matrix, of one of the shortest path

  ## the 0 to Inf
  A = weight_mat
  A[which(A == 0)] = Inf
  diag(A) = 0

  N = dim(A)[1]
  result = matrix(0, N, 1)


  result = t(A[source_node, ])

  ## end_node: the goal space: the nodes except the source node for initial
  ## the space for the nodes which has not found the min distance
  if (source_node == 1) {
    end_node = (source_node + 1):N
  } else if (source_node == N) {
    end_node = 1:(source_node - 1)
  } else {
    end_node = c(1:(source_node - 1), (source_node + 1):N)
  }

  path = matrix(0, N, N)
  path[, 1] = source_node

  if (many_path) {
    p1 = matrix(c(source_node, rep(0, N - 1)), , N)
    ## the path list
    result_path = c()
    for (i in 1:N) {
      result_path[[i]] = p1
    }
  } else {
    path = matrix(0, N, N)
    path[, 1] = source_node

  }
  while (length(end_node) > 0) {

    # id = which(result[end_node] == min(result[end_node]))[1]
    id = which.min(result[end_node])
    ## the new node that has found the min distance ##
    new_id = end_node[id]


    if (many_path) {
      path_mat = result_path[[new_id]]
      path_mat = matrix(path_mat, , N)
      nn = nrow(path_mat)
      mm = ncol(path_mat)
      id_temp_vec = apply(path_mat, 1, which.min)
      id_vec = nn * (id_temp_vec - 1) + matrix(1:nn, nn, 1)
      path_mat[id_vec] = new_id
      result_path[[new_id]] = path_mat
    } else {
      ## the path row for the new_id node, the first element that be 0 should be
      ## updated
      id_temp = which.min(path[new_id, ])
      path[new_id, id_temp] = new_id
    }


    ## delete in the goal space ##
    end_node = end_node[-id]



    ## update the distance vector####

    if (length(end_node) > 0)
    {

      ## update the neighbour node of new_id in the goal space

      update_node = end_node[A[new_id, end_node] != Inf]
      if (length(update_node) > 0) {
        for (i in 1:length(update_node)) {
          temp = result[new_id] + A[new_id, update_node[i]]
          if (temp < result[update_node[i]]) {
            result[update_node[i]] = temp
            ## update the path
            ## path[update_node[i],which.min(path[update_node[i],])[1]]=new_id
            if (many_path) {

              result_path[[update_node[i]]] = result_path[[new_id]]
            } else {
              path[update_node[i], ] = path[new_id, ]
            }
          } else if ((temp == result[update_node[i]]) && many_path) {
            ## if many_path and there is many path actually add one row browser()
            temp_mat = result_path[[update_node[i]]]
            add_row = result_path[[new_id]]
            temp_mat = rbind(temp_mat, add_row)
            result_path[[update_node[i]]] = temp_mat

          }
        }
      }

    }  ## end if length(end_node)>0


  }  ## end while

  if (many_path) {
    path = result_path
  }
  re = list(result, path)

}  ## end function
