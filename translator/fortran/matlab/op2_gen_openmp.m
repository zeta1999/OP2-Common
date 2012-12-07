%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% OpenMP code generator
%
% This routine is called by op2 which parses the input files
%
% It produces a file xxx_kernel.cpp for each kernel,
% plus a master kernel file
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function op2_gen_openmp(master,date,consts,kernels)

global dims idxs typs indtyps inddims
global file FORTRAN CPP g_m g_depth

OP_ID   = 1;  OP_GBL   = 2;  OP_MAP = 3;

OP_READ = 1;  OP_WRITE = 2;  OP_RW  = 3;
OP_INC  = 4;  OP_MAX   = 5;  OP_MIN = 6;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  create new kernel file
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for nk = 1:length(kernels)

  name  = kernels{nk}.name;

  nargs = kernels{nk}.nargs;
  dims  = kernels{nk}.dims;
  maps  = kernels{nk}.maps;
  vars  = kernels{nk}.vars;
  typs  = kernels{nk}.typs;
  accs  = kernels{nk}.accs;
  idxs  = kernels{nk}.idxs;
  inds  = kernels{nk}.inds;

  ninds   = kernels{nk}.ninds;
  inddims = kernels{nk}.inddims;
  indaccs = kernels{nk}.indaccs;
  indtyps = kernels{nk}.indtyps;
  invinds = kernels{nk}.invinds;

%
% set two logicals
%

  ind_inc = max(maps==OP_MAP & accs==OP_INC)  > 0;
  reduct  = max(maps==OP_GBL & accs~=OP_READ) > 0;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  start with CUDA kernel function
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  FORTRAN = 1;
  CPP     = 0;

  file = '';

  g_comm('');
  g_comm('user function');
  g_comm('');
  if FORTRAN g_line(['include ' name '.inc']);
  elseif CPP g_line(['#include ' name '.h']);
  end
  g_comm('');
  g_comm('x86 kernel function');
  g_comm('');

  if FORTRAN prefix = ['subroutine op_x86_' name ];
  elseif CPP prefix = [      'void op_x86_' name ];
  end
  g_line([ prefix ' (   @']);
  prefix = [ '@' blanks(length(prefix)+1)];
  g_depth = 1;

  if (ninds>0)
    if FORTRAN g_line([ prefix 'integer(4) blockIdx, @']);
    elseif CPP g_line([ prefix 'int        blockIdx, @']);
    end
  end

  for g_m = 1:ninds
    if FORTRAN
      g_line([ prefix 'INDTYP *ind_ARG, int *ind_ARG_maps,  @']);
    elseif CPP
      g_line([ prefix 'INDTYP *ind_ARG, int *ind_ARG_maps,  @']);
    end
  end

  for g_m = 1:nargs
    if (maps(g_m)==OP_GBL & accs(g_m)==OP_READ)
      % declared const for performance
      if FORTRAN g_line([ prefix 'ARG,   @']);
      elseif CPP g_line([ prefix 'const TYP *ARG,  @']);
      end
    elseif ( maps(g_m)==OP_ID | maps(g_m)==OP_GBL )
      if FORTRAN g_line([ prefix 'ARG,   @']);
      elseif CPP g_line([ prefix 'TYP *ARG,  @']);
      end
    else
      if FORTRAN g_line([ prefix 'ARG_maps,   @']);
      elseif CPP g_line([ prefix 'short *ARG_maps, @']);
      end
    end
  end

  if (ninds>0)
    if FORTRAN
      g_line('@         ind_arg_sizes, @');
      g_line('@         ind_arg_offs,  @');
      g_line('@         block_offset,  @');
      g_line('@         blkmap,        @');
      g_line('@         offset,        @');
      g_line('@         nelems,        @');
      g_line('@         ncolors,       @');
      g_line('@         colors )        ');
    elseif CPP
      g_line('@  int   *ind_arg_sizes, @');
      g_line('@  int   *ind_arg_offs,  @');
      g_line('@  int    block_offset,  @');
      g_line('@  int   *blkmap,        @');
      g_line('@  int   *offset,        @');
      g_line('@  int   *nelems,        @');
      g_line('@  int   *ncolors,       @');
      g_line('@  int   *colors ) {      ');
    end
  else
    if FORTRAN
      g_line([ prefix 'start, finish )']);
    elseif CPP
      g_line([ prefix 'int start, int finish )']);
    end
  end

  for g_m = 1:nargs
    if (maps(g_m)==OP_MAP & accs(g_m)==OP_INC)
      g_line('TYP ARG_l[DIM]');
    end
  end

%
% lengthy code for general case with indirection
%
  if (ninds>0)
    g_comm('');
    for g_m = 1:ninds
      g_line('int   *ind_ARG_map, ind_ARG_size');
    end
    for g_m = 1:ninds
      g_line('INDTYP *ind_ARG_s');
    end

    if FORTRAN
      g_line('integer(4) :: nelem, offset_b, blockId');
      g_line('character :: shared[64000]');
      g_comm('');
    elseif CPP
      g_line('int  nelem, offset_b, blockId');
      g_line('char shared[64000]');
      g_comm('');
    end
    g_if('0==0');
    g_comm('');
    g_comm(' get sizes and shift pointers and direct-mapped data');
    g_comm('');
    g_line('blockId  = blkmap[blockIdx + block_offset]');
    g_line('nelem    = nelems[blockId]');
    g_line('offset_b = offset[blockId]');
    g_comm('');

    for g_m = 1:ninds
      g_line(['ind_ARG_size = ind_arg_sizes[' ...
             int2str(g_m-1) '+blockId*' int2str(ninds) ']']);
    end
    g_comm('');
    for g_m = 1:ninds
      g_line(['ind_ARG_map = ind_ARG_maps + ind_arg_offs[' ...
             int2str(g_m-1) '+blockId*' int2str(ninds) ']']);
    end

    g_comm('');
    g_comm(' set shared memory pointers');
    g_comm('');
    g_line('nbytes = 0');

    for g_m = 1:ninds
      g_line('ind_ARG_s = (INDTYP *) &shared[nbytes]');
      if (g_m<ninds)
        g_line( ...
        'nbytes    += ROUND_UP(ind_ARG_size*sizeof(INDTYP)*INDDIM)');
      end
    end

    g_endif();

    g_comm('');
    g_comm('copy indirect datasets into shared memory or zero increment');
    g_comm('');

    for g_m = 1:ninds
      if(indaccs(g_m)==OP_READ | indaccs(g_m)==OP_RW | indaccs(g_m)==OP_INC)
        g_for('n','0','INDARG_size');
        g_for('d','0','INDDIM');
        if(indaccs(g_m)==OP_READ | indaccs(g_m)==OP_RW)
          g_line('INDARG_s[d+n*INDDIM] = INDARG[d+INDARG_map[n]*INDDIM]');
        elseif(indaccs(g_m)==OP_INC)
          g_line('INDARG_s[d+n*INDDIM] = ZERO_INDTYP');
        end
        g_endfor();
        g_endfor();
      end
    end

    g_comm('');
    g_comm('process set elements');
    g_comm('');

    if (ind_inc)
      g_for('n','0','nelem');
      g_comm('');
      g_comm('initialise local variables');
      g_comm('');

      for g_m = 1:nargs
        if (maps(g_m)==OP_MAP & accs(g_m)==OP_INC)
          g_for('d','0','DIM');
          g_line('ARG_l[d] = ZERO_TYP');
          g_endfor();
        end
      end

    else
      g_for('n','0','nelem');
    end

%
% simple alternative when no indirection
%
  else

    g_comm('');
    g_comm('process set elements');
    g_comm('');
    g_for('n','start','finish');
  end

%
% kernel call
%

  g_comm('');
  g_comm(' user-supplied kernel call');
  g_comm('');

  for g_m = 1:nargs
    line = ['CALL ' name '( '];

    if (g_m~=1)
      line = ['@' blanks(length(line))];
    end
    if (maps(g_m)==OP_GBL)
      line = [ line 'ARG,' ];
    elseif (maps(g_m)==OP_MAP & accs(g_m)==OP_INC)
      line = [ line 'ARG_l,' ];
    elseif (maps(g_m)==OP_MAP)
      line = [ line 'ind_arg' num2str(inds(g_m)-1) ];
      if FORTRAN line = [ line '(ARG_maps[n+offset_b]*DIM), @' ];
      elseif CPP line = [ line '+ARG_maps[n+offset_b]*DIM, @' ];
      end
    elseif (maps(g_m)==OP_ID)
      if (ninds>0)
        if FORTRAN line = [ line 'ARG((n+offset_b)*DIM),' ];
        elseif CPP line = [ line 'ARG+(n+offset_b)*DIM,' ];
        end
      else
        if FORTRAN line = [ line 'ARG(n*DIM),' ];
        elseif CPP line = [ line 'ARG+n*DIM,' ];
        end
      end

    else
      error('internal error 1')
    end
    if (g_m==nargs)
      line = [ line(1:end-1) ' )' ];
    else
      line = [ line '   @' ];
    end

    g_line(line);
  end

%
% updating for indirect kernels ...
%

  if(ninds>0)
    if(ind_inc)
      g_comm('');
      g_comm('store local variables');
      g_comm('');

      for g_m = 1:nargs
        if (maps(g_m)==OP_MAP & accs(g_m)==OP_INC)
          if FORTRAN
            g_line('ARG_map = ARG_maps[n+offset_b]');
          elseif CPP
            g_line('int ARG_map = ARG_maps[n+offset_b]');
          end
        end
      end

      for g_m = 1:nargs
        if (maps(g_m)==OP_MAP & accs(g_m)==OP_INC)
          g_for('d','0','DIM');
          line = ['ind_arg' num2str(inds(g_m)-1) '_s[d+ARG_map*DIM]'];
          if FORTRAN
            g_line([    line ' =   @']);
            g_line(['@' line ' + ARG_l[d]']);
          elseif CPP
            g_line([line ' += ARG_l[d]']);
          end
          g_endfor();
        end
      end
    end

    g_endfor();

    if(max(indaccs(1:ninds)~=OP_READ)>0)
      g_comm('');
      g_comm('apply pointered write/increment');
      g_comm('');
    end

    for g_m = 1:ninds
      if(indaccs(g_m)==OP_WRITE | indaccs(g_m)==OP_RW | indaccs(g_m)==OP_INC)
        g_for('n','0','INDARG_size');
        g_for('d','0','INDDIM');
        if(indaccs(g_m)==OP_WRITE | indaccs(g_m)==OP_RW)
          g_line('INDARG[d+INDARG_map[n]*INDDIM] = INDARG_s[d+n*INDDIM]');
        elseif(indaccs(g_m)==OP_INC)
          line = 'INDARG[d+INDARG_map[n]*INDDIM]';
          if FORTRAN
            g_line([    line  ' =   @']);
            g_line(['@' line  ' + INDARG_s[d+n*INDDIM]']);
          elseif CPP
            g_line([    line ' += INDARG_s[d+n*INDDIM]']);
          end
        end
        g_endfor();
        g_endfor();
      end
    end
%
% ... and direct kernels
%
  else

    g_endfor();
  end

%
% global reduction
%
  g_comm('');
  g_depth = g_depth-1;
  if FORTRAN g_line(['end subroutine op_x86_' name]);
  elseif CPP g_line('}');
  end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% then C++ stub function
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  g_comm('');
  g_comm('');
  g_comm('host stub function');
  g_comm('');

  if FORTRAN
    prefix = [ 'subroutine op_par_loop_' name ];
    g_line([prefix '( name, set,   @']);
  elseif CPP
    prefix = [ 'void op_par_loop_' name ]
    g_line([prefix '( char const *name, op_set set, @']);
  end

  prefix = [ '@' blanks(length(prefix))];

  g_depth = 1;

  for g_m = 1:nargs
    if FORTRAN
      if (g_m<nargs) g_line([prefix 'ARG,   @']);
      else           g_line([prefix 'ARG )']);
      end
    elseif CPP
      if (g_m<nargs) g_line([prefix 'op_arg ARG,   @']);
      else           g_line([prefix 'op_arg ARG ) {   @']);
      end
    end
  end

  g_comm('');

  for g_m = 1:nargs
    if (maps(g_m)==OP_GBL)
      if FORTRAN g_line('TYP *ARGh = (TYP *)ARG.data');
      elseif CPP g_line('TYP *ARGh = (TYP *)ARG.data');
      end
    end
  end

  g_comm('');

%
%   indirect bits
%
  if (ninds>0)
    g_line(['nargs = ' num2str(nargs)]);
    if FORTRAN
    elseif CPP
      lin = [ 'op_arg args[' num2str(nargs) '] = {'];
      for g_m = 1:nargs
        lin = [lin 'arg' num2str(g_m-1) ','];
      end
      g_line([lin(1:end-1) '}']);
    end
    g_comm('');
    g_line(['ninds = ' num2str(ninds)]);

    if FORTRAN
    elseif CPP
      lin = ['int    inds[' num2str(nargs) '] = {'];
      for g_m = 1:nargs
        lin = strcat(lin,num2str(inds(g_m)-1),',');
      end
      g_line([lin(1:end-1) '}']);
    end

    g_comm('');
    g_if('OP_diags>2');
    if FORTRAN
      g_line(['print " kernel routine with indirection:  ' name ' "']);
    elseif CPP
      g_line(['printf(" kernel routine with indirection:  ' name ' \n")']);
    end
    g_endif();

    g_comm('');
    g_comm('get plan');
    g_comm('');
    g_line(['#ifdef OP_PART_SIZE_'       num2str(nk-1)]);
    g_line(['  part_size = OP_PART_SIZE_' num2str(nk-1)]);
    g_line('#else');
    g_line('  part_size = OP_part_size');
    g_line('#endif');
    line = 'op_plan_get(name,set,part_size,nargs,args,ninds,inds)';
    if FORTRAN g_line(['Plan = ' line ]);
    elseif CPP g_line(['op_plan *Plan = ' line ]);
    end

%
% direct bit
%
  else
    g_if('OP_diags>2');
    if FORTRAN
      g_line(['print " kernel routine w/o indirection: ' name ' "']);
    elseif CPP
      g_line(['printf(" kernel routine w/o indirection: ' name ' \n")']);
    end
    g_endif();
  end

%
% start timing
%
  g_comm('');
  g_comm('initialise timers');
  g_comm('');
  if FORTRAN
    g_line('real(8) :: cpu_t1, cpu_t2, wall_t1, wall_t2');
    g_line('call op_timers(cpu_t1, wall_t1)');
  elseif CPP
    g_line('double cpu_t1, cpu_t2, wall_t1, wall_t2');
    g_line('op_timers(&cpu_t1, &wall_t1)');
  end

%
% set number of threads in x86 execution and create arrays for reduction
%

  g_comm('');
  g_comm('set number of threads');
  g_comm('');
  g_line('#ifdef _OPENMP');
  g_line('nthreads = omp_get_max_threads( )');
  g_line('#else');
  g_line('nthreads = 1');
  g_line('#endif');

  if (reduct)
    g_comm('');
    g_comm('allocate and initialise arrays for global reduction');
    g_comm('');

    for g_m = 1:nargs
      if (maps(g_m)==OP_GBL & accs(g_m)~=OP_READ)
        if FORTRAN
        elseif CPP g_line('  TYP ARG_l[DIM+64*64]');
        end
        g_for('thr','0','nthreads');
        g_for('d','0','DIM');
        if (accs(g_m)==OP_INC)
          g_line('ARG_l[d+thr*64] = ZERO_TYP');
        else
          g_line('ARG_l[d+thr*64] = ARGh[d]');
        end
        g_endfor();
        g_endfor();
      end
    end
  end

%
% kernel call for indirect version
%

  g_comm('');
  g_comm('execute plan');
  g_comm('');

  if (ninds>0)
    g_line('CALL op_mpi_halo_exchange(nargs,args)');
    g_line('block_offset = 0');
    g_for('col','0','Plan->ncolors');
    g_if('col==Plan->ncolors_core');
    g_line('CALL op_mpi_wait_all(nargs,args)');
    g_endif();
    g_line('nblocks = Plan->ncolblk[col]');
    g_line('#pragma omp parallel for');
    g_for('blockIdx','0','nblocks');
    g_line(['CALL op_x86_'  name '( blockIdx,']);

    for m = 1:ninds
      g_m = invinds(m);
      g_line(['(TYP *)ARG.data, Plan->ind_maps[' num2str(m-1) '], @']);
    end

    for g_m = 1:nargs
      if (inds(g_m)==0)
        g_line('(TYP *)ARG.data, @');
      else
        g_line(['Plan->loc_maps[' num2str(g_m-1) '], @']);
      end
    end

    if FORTRAN
    elseif CPP
      g_line('       Plan->ind_sizes,     @');
      g_line('       Plan->ind_offs,      @');
      g_line('       block_offset,        @');
      g_line('       Plan->blkmap,        @');
      g_line('       Plan->offset,        @');
      g_line('       Plan->nelems,        @');
      g_line('       Plan->nthrcol,       @');
      g_line('       Plan->thrcol);        ');
    end
    g_comm('');
    g_line('block_offset = block_offset + nblocks');
    g_endfor();

%
% kernel call for direct version
%
  else
    g_line('#pragma omp parallel for');
    g_for('thr','0','nthreads');
    if FORTRAN
    elseif CPP
      g_line('int start  = (set->size* thr   )/nthreads');
      g_line('int finish = (set->size*(thr+1))/nthreads');
    end

    lin = ['CALL op_x86_' name '( '];
    len = length(lin);

    for g_m = 1:nargs
      if(maps(g_m)==OP_GBL & accs(g_m)~=OP_READ);
        if FORTRAN g_line([lin 'ARG_l(thr*64),   @']);
        elseif CPP g_line([lin 'ARG_l + thr*64,   @']);
        end
      else
        if FORTRAN g_line([lin 'ARG.data, @']);
        elseif CPP g_line([lin '(TYP *) ARG.data, @']);
        end
      end
      lin = [ '@' blanks(len)];
    end

    g_line([ lin 'start, finish )']);
    g_endfor();
  end

%
% combine reduction data from multiple OpenMP threads
%
  g_comm('');
  g_comm('combine reduction data');
  for g_m=1:nargs
    if(maps(g_m)==OP_GBL & accs(g_m)~=OP_READ);
      g_comm('');
      g_for('thr','0','nthreads');
      g_for('d','0','DIM');
      if(accs(g_m)==OP_INC)
        g_line('ARGh[d] = ARGh[d] + ARG_l[d+thr*64]');
      elseif (accs(g_m)==OP_MIN)
        g_line('ARGh[d] = MIN(ARGh[d],ARG_l[d+thr*64])');
      elseif (accs(g_m)==OP_MAX)
        g_line('ARGh[d] = MAX(ARGh[d],ARG_l[d+thr*64])');
      else
        error('internal error: invalid reduction option')
      end
      g_endfor;
      g_endfor;
      g_comm('');
      g_line('CALL op_mpi_reduce(DIM,ARGh)');
    end
  end

%
% update kernel record
%
  g_comm('');
  g_comm('update kernel record');
  g_comm('');

  g_line('CALL op_mpi_barrier()');
  if FORTRAN g_line('CALL op_timers(cpu_t2, wall_t2)');
  elseif CPP g_line('op_timers(&cpu_t2, &wall_t2)');
  end
  g_line(['CALL op_timing_realloc(' num2str(nk-1) ')']);
  kernel = ['OP_kernels[' num2str(nk-1) ']'];
  g_line([kernel '.name      = name']);
  g_line([kernel '.count     = ' kernel '.count + 1']);
  g_line([kernel '.time      = ' kernel '.time  +  wall_t2 - wall_t1']);

  if (ninds>0)
    g_line([kernel '.transfer  = ' kernel '.transfer  +  Plan->transfer']);
    g_line([kernel '.transfer2 = ' kernel '.transfer2 +  Plan->transfer2']);
  else
    g_line([kernel '.transfer  = ' kernel '.transfer   @']);

    ['  OP_kernels[' num2str(nk-1) '].transfer += (float)set->size *'];

    prefix = ['@' blanks(length(kernel)+11)];
    for g_m = 1:nargs
      if(maps(g_m)~=OP_GBL)
        if (accs(g_m)==OP_READ || accs(g_m)==OP_WRITE)
          g_line([prefix '+ set->size * ARG.size   @']);
        else
          g_line([prefix '+ set->size * ARG.size * 2   @']);
        end
      end
    end
    if FORTRAN g_line('@');
    end
  end

  g_depth = 0;
  if FORTRAN g_line(['end subroutine ' name]);
  elseif CPP g_line('} @');
  end
  g_comm('');

  file

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  output individual kernel file
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  fid = fopen(strcat(name,'_kernel.cpp'),'wt');

  fprintf(fid,'// \n// auto-generated by op2.m on %s \n//\n\n',date);
  for n=1:size(file,1)
    lin = file(n,:);
    fprintf(fid,'%s\n',lin);
  end
  fclose(fid);

end  % end of main kernel call loop


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  output one master kernel file
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% change to this below for new version
%     '#include "op_lib_cpp.h"          ',...
%     '#include "op_openmp_rt_support.h"',' ',...

file = strvcat('// header                 ',' ',...
               '#include "op_lib.h"       ',' ',...
               '// global constants       ',' ');

for nc = 1:length(consts)
  if (consts{nc}.dim==1)
    file = strvcat(file, ...
      [ 'extern ' consts{nc}.type ' ' consts{nc}.name ';' ]);
  else
    if (consts{nc}.dim>0)
      num = num2str(consts{nc}.dim);
    else
      num = 'MAX_CONST_SIZE';
    end
    file = strvcat(file, ...
      [ 'extern ' consts{nc}.type ' ' consts{nc}.name '[' num '];' ]);
  end
end

file = strvcat(file,' ','// user kernel files',' ');

for nk = 1:length(kernels)
  file = strvcat(file,...
		 ['#include "' kernels{nk}.name '_kernel.cpp"']);
end

fid = fopen([ master '_kernels.cpp'],'wt');

fprintf(fid,'// \n// auto-generated by op2.m on %s \n//\n\n',date);

for n=1:size(file,1)
  fprintf(fid,'%s\n',file(n,:));
end

fclose(fid);

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% a little function to replace keywords
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function line = rep(line,m)

global dims idxs typs indtyps inddims

if regexp(line,'INDDIM')
  line = regexprep(line,'INDDIM',inddims(m));
end

if regexp(line,'INDARG')
  line = regexprep(line,'INDARG',['ind_arg' num2str(m-1)]);
end

if regexp(line,'ZERO_INDTYP')
  type = regexprep(regexprep(indtyps{m},'(',' '),')',' ');
  line = regexprep(line,'ZERO_INDTYP',['ZERO_' type]);
end

if regexp(line,'INDTYP')
  line = regexprep(line,'INDTYP',indtyps(m));
end

if regexp(line,'DIM')
  line = regexprep(line,'DIM',dims(m));
end

if regexp(line,'ARG')
  line = regexprep(line,'ARG',['arg' num2str(m-1)]);
end

if regexp(line,'ZERO_TYP')
  type = regexprep(regexprep(typs{m},'(',''),')','');
  line = regexprep(line,'ZERO_TYP',['ZERO_' type]);
end

if regexp(line,'TYP')
  line = regexprep(line,'TYP',typs(m));
end

if regexp(line,'IDX')
  line = regexprep(line,'IDX',num2str(idxs(m)));
end

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% a little function to replace keywords
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% first a function to insert comments
%

function g_comm(line)

global file FORTRAN CPP

if length(line)==0
  file = strvcat(file,' ');
else
  if FORTRAN file = strvcat(file,['!  ' line]);
  elseif CPP file = strvcat(file,['// ' line]);
  end
end

end

%
% now a function to insert lines of code ...
%

function g_line(line)

global file FORTRAN CPP g_m g_depth

% first do string substitution

line = rep(line,g_m);

% add whitespace dependent on depth

if strcmp(line(1),'@')
  line = [line(1) blanks(2*g_depth-1) line(2:end)];
elseif ~strcmp(line(1),'#')
  line = [blanks(2*g_depth) line];
end

% now do language specific bits

if FORTRAN
  line = regexprep(line,'CALL','call');
  line = regexprep(line,'[','(');
  line = regexprep(line,']',')');
  line = regexprep(line,'@','&');
  file = strvcat(file,line);
elseif CPP
  line = regexprep(line,'CALL ','');
  if ~( strcmp(line(end),'@') | strcmp(line(1),'#') )
    line = [line ';'];
  end
  line = regexprep(line,'@','');
  file = strvcat(file,line);
end

end

%
% ... and various specialised versions
%

function g_for(i,start,finish)

global file FORTRAN CPP g_m g_depth

if FORTRAN
  g_line([ 'do ' i ' = ' start ', ' finish '-1']);
elseif CPP
  g_line([ 'for ( ' i '=' start '; i<' finish '; ' i '++']);
end

g_depth = g_depth+1;

end


function g_endfor()

global file FORTRAN CPP g_m g_depth

g_depth = g_depth-1;

if FORTRAN
  g_line('enddo');
elseif CPP
  g_line('}   @');
end

end


function g_if(line)

global file FORTRAN CPP g_m g_depth

if FORTRAN
  g_line(['if (' line ') then']);
elseif CPP
  g_line(['if (' line ') {  @']);
end

g_depth = g_depth+1;

end


function g_endif

global file FORTRAN CPP g_m g_depth

g_depth = g_depth-1;

if FORTRAN
  g_line('endif');
elseif CPP
  g_line('}  @');
end
end
