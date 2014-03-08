##########################################################################
#
# MPI Sequential code generator
#
# This routine is called by op2 which parses the input files
#
# It produces a file xxx_kernel.cpp for each kernel,
# plus a master kernel file
#
##########################################################################

import re
import datetime

def comm(line):
  global file_text, FORTRAN, CPP
  global depth
  prefix = ' '*depth
  if len(line) == 0:
    file_text +='\n'
  elif FORTRAN:
    file_text +='!  '+line+'\n'
  elif CPP:
    file_text +=prefix+'//'+line.rstrip()+'\n'

def macro(line):
  global file_text, FORTRAN, CPP
  global depth
  file_text +=line+'\n'

def rep(line,m):
  global dims, idxs, typs, indtyps, inddims
  if m < len(inddims):
    line = re.sub('INDDIM',str(inddims[m]),line)
    line = re.sub('INDTYP',str(indtyps[m]),line)

  line = re.sub('INDARG','ind_arg'+str(m),line)
  line = re.sub('DIM',str(dims[m]),line)
  line = re.sub('ARG','arg'+str(m),line)
  line = re.sub('TYP',typs[m],line)
  line = re.sub('IDX',str(int(idxs[m])),line)
  return line

def code(text):
  global file_text, FORTRAN, CPP, g_m
  global depth
  if text == '':
    prefix = ''
  else:
    prefix = ' '*depth
  file_text += prefix+rep(text,g_m).rstrip()+'\n'

def FOR(i,start,finish):
  global file_text, FORTRAN, CPP, g_m
  global depth
  if FORTRAN:
    code('do '+i+' = '+start+', '+finish+'-1')
  elif CPP:
    code('for ( int '+i+'='+start+'; '+i+'<'+finish+'; '+i+'++ ){')
  depth += 2

def ENDFOR():
  global file_text, FORTRAN, CPP, g_m
  global depth
  depth -= 2
  if FORTRAN:
    code('enddo')
  elif CPP:
    code('}')

def IF(line):
  global file_text, FORTRAN, CPP, g_m
  global depth
  if FORTRAN:
    code('if ('+line+') then')
  elif CPP:
    code('if ('+ line + ') {')
  depth += 2

def ENDIF():
  global file_text, FORTRAN, CPP, g_m
  global depth
  depth -= 2
  if FORTRAN:
    code('endif')
  elif CPP:
    code('}')


def op2_gen_openmp_vector_color2(master, date, consts, kernels):

  global dims, idxs, typs, indtyps, inddims
  global FORTRAN, CPP, g_m, file_text, depth

  OP_ID   = 1;  OP_GBL   = 2;  OP_MAP = 3;

  OP_READ = 1;  OP_WRITE = 2;  OP_RW  = 3;
  OP_INC  = 4;  OP_MAX   = 5;  OP_MIN = 6;

  accsstring = ['OP_READ','OP_WRITE','OP_RW','OP_INC','OP_MAX','OP_MIN' ]

  any_soa = 0
  for nk in range (0,len(kernels)):
    any_soa = any_soa or sum(kernels[nk]['soaflags'])

##########################################################################
#  create new kernel file
##########################################################################

  for nk in range (0,len(kernels)):
    name  = kernels[nk]['name']
    nargs = kernels[nk]['nargs']
    dims  = kernels[nk]['dims']
    maps  = kernels[nk]['maps']
    var   = kernels[nk]['var']
    typs  = kernels[nk]['typs']
    accs  = kernels[nk]['accs']
    idxs  = kernels[nk]['idxs']
    inds  = kernels[nk]['inds']
    soaflags = kernels[nk]['soaflags']

    ninds   = kernels[nk]['ninds']
    inddims = kernels[nk]['inddims']
    indaccs = kernels[nk]['indaccs']
    indtyps = kernels[nk]['indtyps']
    invinds = kernels[nk]['invinds']
    mapnames = kernels[nk]['mapnames']
    invmapinds = kernels[nk]['invmapinds']
    mapinds = kernels[nk]['mapinds']
    nmaps = 0
    if ninds > 0:
      nmaps = max(mapinds)+1

    vec =  [m for m in range(0,nargs) if int(idxs[m])<0 and maps[m] == OP_MAP]

    vector_bitlength = 512
    vector_size = vector_bitlength/8/4
    has_doubles = 0
    for g_m in range (0,nargs):
      if (typs[g_m] == 'double'):
        vector_size = vector_bitlength/8/8
        has_doubles = 1

    vectyps = nargs*['']
    veclen = nargs*[0]
    for g_m in range (0,nargs):
      if (typs[g_m] == 'int'):
        if has_doubles:
          vectyps[g_m] = 'intv_half'
          veclen[g_m] = vector_size/2
        else:
          vectyps[g_m] = 'intv'
          veclen[g_m] = vector_size
      if (typs[g_m] == 'float'):
        if has_doubles:
          print 'Error: mixed precision support not implemented'
        else:
          vectyps[g_m] = 'floatv'
          veclen[g_m] = vector_size
      if (typs[g_m] == 'double'):
        vectyps[g_m] = 'doublev'
        veclen[g_m] = vector_size

    if has_doubles:
      vector_size = 'VECSIZEH'
    else:
      vector_size = 'VECSIZE'

    if len(vec) > 0:
      unique_args = [1];
      vec_counter = 1;
      vectorised = []
      new_dims = []
      new_maps = []
      new_vars = []
      new_typs = []
      new_accs = []
      new_idxs = []
      new_inds = []
      new_soaflags = []
      for m in range(0,nargs):
          if int(idxs[m])<0 and maps[m] == OP_MAP:
            if m > 0:
              unique_args = unique_args + [len(new_dims)+1]
            temp = [0]*(-1*int(idxs[m]))
            for i in range(0,-1*int(idxs[m])):
              temp[i] = var[m]
            new_vars = new_vars+temp
            for i in range(0,-1*int(idxs[m])):
              temp[i] = typs[m]
            new_typs = new_typs+temp
            for i in range(0,-1*int(idxs[m])):
              temp[i] = dims[m]
            new_dims = new_dims+temp
            new_maps = new_maps+[maps[m]]*int(-1*int(idxs[m]))
            new_soaflags = new_soaflags+[0]*int(-1*int(idxs[m]))
            new_accs = new_accs+[accs[m]]*int(-1*int(idxs[m]))
            for i in range(0,-1*int(idxs[m])):
              new_idxs = new_idxs+[i]
            new_inds = new_inds+[inds[m]]*int(-1*int(idxs[m]))
            vectorised = vectorised + [vec_counter]*int(-1*int(idxs[m]))
            vec_counter = vec_counter + 1;
          else:
            if m > 0:
              unique_args = unique_args + [len(new_dims)+1]
            new_dims = new_dims+[dims[m]]
            new_maps = new_maps+[maps[m]]
            new_accs = new_accs+[int(accs[m])]
            new_soaflags = new_soaflags+[soaflags[m]]
            new_idxs = new_idxs+[int(idxs[m])]
            new_inds = new_inds+[inds[m]]
            new_vars = new_vars+[var[m]]
            new_typs = new_typs+[typs[m]]
            vectorised = vectorised+[0]
      dims = new_dims
      maps = new_maps
      accs = new_accs
      idxs = new_idxs
      inds = new_inds
      var = new_vars
      typs = new_typs
      soaflags = new_soaflags;
      nargs = len(vectorised);

      for i in range(1,ninds+1):
        for index in range(0,len(inds)+1):
          if inds[index] == i:
            invinds[i-1] = index
            break
    else:
      vectorised = [0]*nargs
      unique_args = range(1,nargs+1)

    cumulative_indirect_index = [-1]*nargs;
    j = 0;
    for i in range (0,nargs):
      if maps[i] == OP_MAP:
        cumulative_indirect_index[i] = j
        j = j + 1
#
# set two logicals
#
    j = 0
    for i in range(0,nargs):
      if maps[i] == OP_MAP and accs[i] == OP_INC:
        j = i
    ind_inc = j > 0

    j = 0
    for i in range(0,nargs):
      if maps[i] == OP_GBL and accs[i] <> OP_READ:
        j = i
    reduct = j > 0

##########################################################################
#  start with the user kernel function
##########################################################################

    FORTRAN = 0;
    CPP     = 1;
    g_m = 0;
    file_text = ''
    depth = 0

    comm('user function')
    if FORTRAN:
      code('include '+name+'.inc')
    elif CPP:
      code('#include "'+name+'.h"')

    fid = open(name+'.h')
    text = fid.read()
    fid.close()
    text = text.replace(name,name+'_vec')
    if has_doubles:
      for g_m in range(0,nargs):
        if typs[g_m] == 'double':
          text = re.sub('\\bdouble\\b','doublev',text);
        elif typs[g_m] == 'float':
          text = re.sub('\\bfloat\\b','floatv_half',text);
        elif typs[g_m] == 'int':
          text = re.sub('\\bint\\b','intv_half',text);
    else:
      for g_m in range(0,nargs):
        if typs[g_m] == 'float':
          text = re.sub('\\bfloat\\b','floatv',text);
        elif typs[g_m] == 'int':
          text = re.sub('\\bint\\b','intv',text);
    macro('#ifdef VECTORIZE')
    file_text = file_text + text
    macro('\n#endif')
##########################################################################
# then C++ stub function
##########################################################################

    code('')
    comm(' host stub function')
    code('void op_par_loop_'+name+'(char const *name, op_set set,')
    depth += 2

    for m in unique_args:
      g_m = m - 1
      if m == unique_args[len(unique_args)-1]:
        code('op_arg ARG){');
        code('')
      else:
        code('op_arg ARG,')

    for g_m in range (0,nargs):
      if maps[g_m]==OP_GBL and accs[g_m] <> OP_READ:
        code('TYP *ARGh = (TYP *)ARG.data;')

    code('int nargs = '+str(nargs)+';')
    code('op_arg args['+str(nargs)+'];')
    code('')

    for g_m in range (0,nargs):
      u = [i for i in range(0,len(unique_args)) if unique_args[i]-1 == g_m]
      if len(u) > 0 and vectorised[g_m] > 0:
        code('ARG.idx = 0;')
        code('args['+str(g_m)+'] = ARG;')

        v = [int(vectorised[i] == vectorised[g_m]) for i in range(0,len(vectorised))]
        first = [i for i in range(0,len(v)) if v[i] == 1]
        first = first[0]

        FOR('v','1',str(sum(v)))
        code('args['+str(g_m)+' + v] = op_arg_dat(arg'+str(first)+'.dat, v, arg'+\
        str(first)+'.map, DIM, "TYP", '+accsstring[accs[g_m]-1]+');')
        ENDFOR()
        code('')
      elif vectorised[g_m]>0:
        pass
      else:
        code('args['+str(g_m)+'] = ARG;')

    if ninds>0:
      code('int  ninds   = '+str(ninds)+';')
      line = 'int  inds['+str(nargs)+'] = {'
      for m in range(0,nargs):
        line += str(inds[m]-1)+','
      code(line[:-1]+'};')
      code('')
      code('#ifdef OP_PART_SIZE_'+ str(nk))
      code('  int part_size = OP_PART_SIZE_'+str(nk)+';')
      code('#else')
      code('  int part_size = OP_part_size;')
      code('#endif')
      code('')
    comm(' set number of threads')
    code('#ifdef _OPENMP')
    code('  int nthreads = omp_get_max_threads();')
    code('#else')
    code('  int nthreads = 1;')
    code('#endif')

    if reduct:
      code('')
      comm(' allocate and initialise arrays for global reduction')
      for g_m in range(0,nargs):
        if maps[g_m]==OP_GBL and accs[g_m]<>OP_READ:
          code('TYP ARG_red[nthreads*64];')
          FOR('thr','0','nthreads')
          if accs[g_m]==OP_INC:
            FOR('d','0','DIM')
            code('ARG_red[d+thr*64]=ZERO_TYP;')
            ENDFOR()
          else:
            FOR('d','0','DIM')
            code('ARG_red[d+thr*64]=ARGh[d];')
            ENDFOR()
          ENDFOR()
#
# start timing
#
    code('')
    comm(' initialise timers')
    code('double cpu_t1, cpu_t2, wall_t1, wall_t2;')
    code('op_timing_realloc('+str(nk)+');')
    code('op_timers_core(&cpu_t1, &wall_t1);')
    code('')

    code('')
    code('int exec_size = op_mpi_halo_exchanges(set, nargs, args);')
    code('int set_size = ((set->size+set->exec_size-1)/16+1)*16; //align to 512 bits')

#
#   indirect bits
#
    if ninds>0:
      code('op_plan *Plan = op_plan_get_stage(name,set,part_size,nargs,args,ninds,inds, OP_COLOR2);')
      IF('OP_diags>2')
      code('printf(" kernel routine with indirection: '+name+'\\n");')
      ENDIF()
#
# direct bit
#
    else:
      code('')
      IF('OP_diags>2')
      code('printf(" kernel routine w/o indirection:  '+ name + '");')
      ENDIF()

    code('')
    IF('exec_size >0')
    code('')

#
# kernel call for indirect version
#
    if ninds>0:
      comm(' execute plan')
      code('int block_offset = 0;')
      FOR('col','0','Plan->ncolors')
      IF('col==1')
      code('op_mpi_wait_all(nargs, args);')
      ENDIF()
      code('int start = Plan->thrcol[col];')
      code('int end = Plan->thrcol[col+1];')
      code('')
      macro('#ifdef VECTORIZE')
      comm('assumes blocksize is multiple of vector size')

      #first bit, getting to divisible by vector_size
      code('int presweep = min(((start-1)/'+str(vector_size)+'+1)*'+str(vector_size)+',end);')
      FOR('e','start','presweep')
      code('int n = plan->col_reord[e];')
      if nmaps > 0:
        k = []
        for g_m in range(0,nargs):
          if maps[g_m] == OP_MAP and (not mapinds[g_m] in k):
            k = k + [mapinds[g_m]]
            code('int map'+str(mapinds[g_m])+'idx = arg'+str(invmapinds[inds[g_m]-1])+'.map_data[n * arg'+str(invmapinds[inds[g_m]-1])+'.map->dim + '+idxs[g_m]+'];')
      code('')
      line = name+'('
      indent = '\n'+' '*(depth+2)
      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID:
          line = line + indent + '&(('+typs[g_m]+'*)arg'+str(g_m)+'.data)['+dims[g_m]+' * n]'
        if maps[g_m] == OP_MAP:
          line = line + indent + '&(('+typs[g_m]+'*)arg'+str(invinds[inds[g_m]-1])+'.data)['+dims[g_m]+' * map'+str(mapinds[g_m])+'idx]'
        if maps[g_m] == OP_GBL and accs[g_m]<> OP_READ:
          line = line + indent +'&arg'+str(g_m)+'_red[64*thr]'
        if maps[g_m] == OP_GBL and accs[g_m]== OP_READ:
          line = line + indent +'('+typs[g_m]+'*)arg'+str(g_m)+'.data'
        if g_m < nargs-1:
          line = line +','
        else:
           line = line +');'
      code(line)
      ENDFOR()



      FOR('e','presweep/'+str(vector_size),'end/'+str(vector_size))
      if has_doubles:
        code('intv_half n(&Plan->col_reord['+str(vector_size)+'*n ]);')
      else:
        code('intv n(&Plan->col_reord['+str(vector_size)+'*n ]);')

      if nmaps > 0:
        k = []
        for g_m in range(0,nargs):
          if maps[g_m] == OP_MAP and (not mapinds[g_m] in k):
            k = k + [mapinds[g_m]]
            if has_doubles:
              code('intv_half map'+str(mapinds[g_m])+'idx(&arg'+str(invmapinds[inds[g_m]-1])+'.map_data_d[set_size * '+idxs[g_m]+'], n);')
            else:
              code('intv map'+str(mapinds[g_m])+'idx(&arg'+str(invmapinds[inds[g_m]-1])+'.map_data[set_size * '+idxs[g_m]+'], n);')
      code('')
      #
      # Gather
      #

      for g_m in range(0,nargs):
        if maps[g_m] == OP_MAP and dims[g_m]>1:
          if has_doubles:
            code('intv_half mapidx;')
          else:
            code('intv mapidx;')
          break

      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID:
          #this might not be safe, if not all fields are written!
          if accs[g_m] == OP_WRITE:
            line = vectyps[g_m]+' ARG_p[DIM];'
          else:
            line = vectyps[g_m]+' ARG_p[DIM] = {'
            if int(dims[g_m])==1:
              line = line + vectyps[g_m]+'(('+typs[g_m]+'*)arg'+str(g_m)+'.data, n)};'
            else:
              indent = '\n'+' '*(depth+2)
              for d in range(0,int(dims[g_m])):
                line = line + indent + vectyps[g_m]+'(('+typs[g_m]+'*)arg'+str(g_m)+'.data +'+str(d)+', '+dims[g_m]+'*n)'
                if d < int(dims[g_m])-1:
                  line = line +','
                else:
                   line = line +'};'
          code(line)
        if maps[g_m] == OP_MAP and accs[g_m] == OP_INC:
          line = vectyps[g_m]+' ARG_p[DIM] = {'
          indent = '\n'+' '*(depth+2)
          for d in range(0,int(dims[g_m])):
            if (typs[g_m] == 'int'):
              line = line + indent +vectyps[g_m]+'(0)'
            elif (typs[g_m] == 'float'):
              line = line + indent +vectyps[g_m]+'(0.0f)'
            else:
              line = line + indent +vectyps[g_m]+'(0.0)'
            if d < int(dims[g_m])-1:
              line = line +','
            else:
               line = line +'};'
          code(line)
        if maps[g_m] == OP_MAP and accs[g_m] <> OP_INC:
          mapidx = 'map'+str(mapinds[g_m])+'idx'
          if int(dims[g_m])>1:
            code('mapidx = '+dims[g_m]+'*map'+str(mapinds[g_m])+'idx;')
            mapidx = 'mapidx'

          line = vectyps[g_m]+' ARG_p[DIM] = {'
          indent = '\n'+' '*(depth+2)
          for d in range(0,int(dims[g_m])):
            line = line + indent + vectyps[g_m]+'(('+typs[g_m]+'*)arg'+str(g_m)+'.data+'+str(d)+', '+mapidx+')'
            if d < int(dims[g_m])-1:
              line = line +','
            else:
                line = line +'};'
          code(line)
        if maps[g_m] == OP_GBL and (accs[g_m]==OP_READ or accs[g_m]==OP_MIN or accs[g_m]==OP_MAX):
          code(vectyps[g_m]+' ARG_l(*('+typs[g_m]+'*)arg'+str(g_m)+'.data);')
        if maps[g_m] == OP_GBL and accs[g_m]==OP_INC:
          if (typs[g_m] == 'int'):
            code(vectyps[g_m]+' ARG_l(0);')
          elif (typs[g_m] == 'float'):
            code(vectyps[g_m]+' ARG_l(0.0f);')
          else:
            code(vectyps[g_m]+' ARG_l(0.0);')

      #
      # Call kernel
      #
      line = name+'_vec('
      indent = '\n'+' '*(depth+2)
      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID:
          line = line + indent + 'arg'+str(g_m)+'_p'
        if maps[g_m] == OP_MAP:
          line = line + indent + 'arg'+str(g_m)+'_p'
        if maps[g_m] == OP_GBL:
          line = line + indent + '&arg'+str(g_m)+'_l'
        if g_m < nargs-1:
          line = line +','
        else:
           line = line +');'
      code(line)

      #
      # Write back
      #
      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID and accs[g_m] <> OP_READ:
          if int(dims[g_m])==1:
            code('store_a(ARG_p[0],&(('+typs[g_m]+'*)arg'+str(g_m)+'.data)['+str(vector_size)+'* n]);')
          else:
            for d in range(0,int(dims[g_m])):
              code('store_stride(ARG_p['+str(d)+'],&(('+typs[g_m]+'*)arg'+str(g_m)+'.data)['+str(vector_size)+'*'+dims[g_m]+' * n +'+str(d)+'], '+dims[g_m]+');')
        if maps[g_m] == OP_MAP and accs[g_m] <> OP_READ:
          mapidx = 'map'+str(mapinds[g_m])+'idx'
          if dims[g_m]>1:
            code('mapidx = '+dims[g_m]+'*map'+str(mapinds[g_m])+'idx;')
            mapidx = 'mapidx'
          if accs[g_m] == OP_INC:
            kind = '_add'
          for d in range(0,int(dims[g_m])):
            code('store_scatter_safe'+kind+'(ARG_p['+str(d)+'], ('+typs[g_m]+'*)arg'+str(g_m)+'.data+'+str(d)+', '+mapidx+');')
        if maps[g_m] == OP_GBL:
          if accs[g_m]==OP_INC:
            code('arg'+str(g_m)+'_red[64*thr] += add_horizontal(ARG_l);')
          if accs[g_m]==OP_MIN:
            code('arg'+str(g_m)+'_red[64*thr] = min(arg'+str(g_m)+'_red[64*thr],min_horizontal(ARG_l));')

      ENDFOR()

      #last bit, not divisible by vector_size
      code('int postsweep = max((end/'+str(vector_size)+')*'+str(vector_size)+', presweep);')
      FOR('e','postsweep','end')
      depth -=2
      macro('#else')
      FOR('e','start','end')
      macro('#endif')
      code('int n = Plan->col_reord[e];')
      if nmaps > 0:
        k = []
        for g_m in range(0,nargs):
          if maps[g_m] == OP_MAP and (not mapinds[g_m] in k):
            k = k + [mapinds[g_m]]
            code('int map'+str(mapinds[g_m])+'idx = arg'+str(invmapinds[inds[g_m]-1])+'.map_data[n * arg'+str(invmapinds[inds[g_m]-1])+'.map->dim + '+idxs[g_m]+'];')
      code('')
      line = name+'('
      indent = '\n'+' '*(depth+2)
      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID:
          line = line + indent + '&(('+typs[g_m]+'*)arg'+str(g_m)+'.data)['+dims[g_m]+' * n]'
        if maps[g_m] == OP_MAP:
          line = line + indent + '&(('+typs[g_m]+'*)arg'+str(invinds[inds[g_m]-1])+'.data)['+dims[g_m]+' * map'+str(mapinds[g_m])+'idx]'
        if maps[g_m] == OP_GBL and accs[g_m]<> OP_READ:
          line = line + indent +'&arg'+str(g_m)+'_red[64*thr]'
        if maps[g_m] == OP_GBL and accs[g_m]== OP_READ:
          line = line + indent +'('+typs[g_m]+'*)arg'+str(g_m)+'.data'
        if g_m < nargs-1:
          line = line +','
        else:
           line = line +');'
      code(line)
      ENDFOR()
      ENDFOR()
      code('block_offset += nblocks;');
      #
      # combine reduction data from multiple OpenMP threads
      #
      comm(' combine reduction data')
      for g_m in range(0,nargs):
        if maps[g_m]==OP_GBL and accs[g_m]<>OP_READ:
          FOR('thr','0','nthreads')
          if accs[g_m]==OP_INC:
            FOR('d','0','DIM')
            code('ARGh[d] += ARG_red[d+thr*64];')
            ENDFOR()
          elif accs[g_m]==OP_MIN:
            FOR('d','0','DIM')
            code('ARGh[d]  = MIN(ARGh[d],ARG_red[d+thr*64]);')
            ENDFOR()
          elif accs[g_m]==OP_MAX:
            FOR('d','0','DIM')
            code('ARGh[d]  = MAX(ARGh[d],ARG_red[d+thr*64]);')
            ENDFOR()
          else:
            print 'internal error: invalid reduction option'
          ENDFOR()
          code('op_mpi_reduce(&ARG,ARGh);')

      ENDFOR()

#
# kernel call for direct version
#
    else:
      code('int chunk_size = (((set->size-1)/nthreads+1-1)/16+1)*16;')
      code('#pragma omp parallel for')
      FOR('thr','0','nthreads')
      code('int start  = min(thr*chunk_size,set->size);')
      code('int finish = min((thr+1)*chunk_size,set->size);')
      macro('#ifdef VECTORIZE')
      FOR('n','start/'+str(vector_size),'finish/'+str(vector_size))
      #
      # Gather
      #
      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID:
          #this might not be safe, if not all fields are written!
          if accs[g_m] == OP_WRITE:
            line = vectyps[g_m]+' ARG_p[DIM];'
          else:
            line = vectyps[g_m]+' ARG_p[DIM] = {'
            if int(dims[g_m])==1:
              line = line + vectyps[g_m]+'(&(('+typs[g_m]+'*)arg'+str(g_m)+'.data)['+str(vector_size)+' * n])};'
            else:
              indent = '\n'+' '*(depth+2)
              for d in range(0,int(dims[g_m])):
                line = line + indent + vectyps[g_m]+'(&(('+typs[g_m]+'*)arg'+str(g_m)+'.data)['+str(vector_size)+'*'+dims[g_m]+' * n +'+str(d)+'], '+dims[g_m]+')'
                if d < int(dims[g_m])-1:
                  line = line +','
                else:
                   line = line +'};'
          code(line)
        if maps[g_m] == OP_GBL and (accs[g_m]==OP_READ or accs[g_m]==OP_MIN or accs[g_m]==OP_MAX):
          code(vectyps[g_m]+' ARG_l(*('+typs[g_m]+'*)arg'+str(g_m)+'.data);')
        if maps[g_m] == OP_GBL and accs[g_m]==OP_INC:
          if (typs[g_m] == 'int'):
            code(vectyps[g_m]+' ARG_l(0);')
          elif (typs[g_m] == 'float'):
            code(vectyps[g_m]+' ARG_l(0.0f);')
          else:
            code(vectyps[g_m]+' ARG_l(0.0);')

      #
      # Call kernel
      #
      line = name+'_vec('
      indent = '\n'+' '*(depth+2)
      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID:
          line = line + indent + 'arg'+str(g_m)+'_p'
        if maps[g_m] == OP_GBL:
          line = line + indent + '&arg'+str(g_m)+'_l'
        if g_m < nargs-1:
          line = line +','
        else:
           line = line +');'
      code(line)

      #
      # Write back
      #
      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID and accs[g_m] <> OP_READ:
          if int(dims[g_m])==1:
            code('store_a(ARG_p[0],&(('+typs[g_m]+'*)arg'+str(g_m)+'.data)['+str(vector_size)+'* n]);')
          else:
            for d in range(0,int(dims[g_m])):
              code('store_stride(ARG_p['+str(d)+'],&(('+typs[g_m]+'*)arg'+str(g_m)+'.data)['+str(vector_size)+'*'+dims[g_m]+' * n +'+str(d)+'], '+dims[g_m]+');')
        if maps[g_m] == OP_GBL:
          if accs[g_m]==OP_INC:
            code('arg'+str(g_m)+'_red[64*thr] += add_horizontal(ARG_l);')
          if accs[g_m]==OP_MIN:
            code('arg'+str(g_m)+'_red[64*thr] = min(arg'+str(g_m)+'_red[64*thr],min_horizontal(ARG_l));')

      ENDFOR()
      #last bit, not divisible by vector_size
      FOR('n','(finish/'+str(vector_size)+')*'+str(vector_size),'finish')
      depth-=2
      macro('#else')
      FOR('n','start','finish')
      macro('#endif')
      line = name+'('
      indent = '\n'+' '*(depth+2)
      for g_m in range(0,nargs):
        if maps[g_m] == OP_ID:
          line = line + indent + '&(('+typs[g_m]+'*)arg'+str(g_m)+'.data)['+dims[g_m]+'*n]'
        if maps[g_m] == OP_GBL and accs[g_m] <> OP_READ:
          line = line + indent +'&arg'+str(g_m)+'_red[64*thr]'
        if maps[g_m] == OP_GBL and accs[g_m] == OP_READ:
          line = line + indent +'('+typs[g_m]+'*)arg'+str(g_m)+'.data'
        if g_m < nargs-1:
          line = line +','
        else:
           line = line +');'
      code(line)
      ENDFOR()
      ENDFOR()
    ENDIF()
    code('')

    #zero set size issues
    if ninds>0:
      IF('exec_size == 0 || exec_size == set->core_size')
      code('op_mpi_wait_all(nargs, args);')
      ENDIF()

#
# combine reduction data from multiple OpenMP threads
#
    if ninds == 0:
      comm(' combine reduction data')
      for g_m in range(0,nargs):
        if maps[g_m]==OP_GBL and accs[g_m]<>OP_READ:
          FOR('thr','0','nthreads')
          if accs[g_m]==OP_INC:
            FOR('d','0','DIM')
            code('ARGh[d] += ARG_red[d+thr*64];')
            ENDFOR()
          elif accs[g_m]==OP_MIN:
            FOR('d','0','DIM')
            code('ARGh[d]  = MIN(ARGh[d],ARG_red[d+thr*64]);')
            ENDFOR()
          elif accs[g_m]==OP_MAX:
            FOR('d','0','DIM')
            code('ARGh[d]  = MAX(ARGh[d],ARG_red[d+thr*64]);')
            ENDFOR()
          else:
            print 'internal error: invalid reduction option'
          ENDFOR()
          code('op_mpi_reduce(&ARG,ARGh);')

    code('op_mpi_set_dirtybit(nargs, args);')
    code('')

#
# update kernel record
#

    comm(' update kernel record')
    code('op_timers_core(&cpu_t2, &wall_t2);')
    code('OP_kernels[' +str(nk)+ '].name      = name;')
    code('OP_kernels[' +str(nk)+ '].count    += 1;')
    code('OP_kernels[' +str(nk)+ '].time     += wall_t2 - wall_t1;')

    if ninds == 0:
      line = 'OP_kernels['+str(nk)+'].transfer += (float)set->size *'

      for g_m in range (0,nargs):
        if maps[g_m]<>OP_GBL:
          if accs[g_m]==OP_READ:
            code(line+' ARG.size;')
          else:
            code(line+' ARG.size * 2.0f;')

    depth -= 2
    code('}')


##########################################################################
#  output individual kernel file
##########################################################################
    fid = open(name+'_kernel.cpp','w')
    date = datetime.datetime.now()
    fid.write('//\n// auto-generated by op2.py on '+date.strftime("%Y-%m-%d %H:%M")+'\n//\n\n')
    fid.write(file_text)
    fid.close()

# end of main kernel call loop


##########################################################################
#  output one master kernel file
##########################################################################

  file_text =''
  comm(' header                 ')
  code('#include "op_lib_cpp.h" ')
  macro("#ifdef VECTORIZE")
  code('#include "op_vector.h"  ')
  macro('#endif')
  code('')
  comm(' global constants       ')

  for nc in range (0,len(consts)):
    if consts[nc]['dim']==1:
      code('extern '+consts[nc]['type'][1:-1]+' '+consts[nc]['name']+';')
    else:
      if consts[nc]['dim'] > 0:
        num = str(consts[nc]['dim'])
      else:
        num = 'MAX_CONST_SIZE'

      code('extern '+consts[nc]['type'][1:-1]+' '+consts[nc]['name']+'['+num+'];')

  if any_soa:
    code('')
    code('extern int op2_stride;')
    code('#define OP2_STRIDE(arr, idx) arr[idx]')
    code('')

  comm(' user kernel files')

  for nk in range(0,len(kernels)):
    code('#include "'+kernels[nk]['name']+'_kernel.cpp"')
  master = master.split('.')[0]
  fid = open(master.split('.')[0]+'_kernels.cpp','w')
  fid.write('//\n// auto-generated by op2.py on '+date.strftime("%Y-%m-%d %H:%M")+'\n//\n\n')
  fid.write(file_text)
  fid.close()