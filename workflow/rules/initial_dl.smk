"""
snakemake -n -R ncbi_download

# batch-job submission (offline)
snakemake --jobs 10 \
  --latency-wait 30 \
  --use-conda \
  -p \
  --default-resources mem_mb=25600 \
  --cluster '
    qsub \
      -V -cwd \
      -P fair_share \
      -l idle=1 \
      -l si_flag=1 \
      -pe multislot {threads} \
      -l vf={resources.mem_mb}' \
      --jn job.{name}.{jobid}.sh \
      -R ncbi_download && mv job.* logs/

# local submission
snakemake --cores 6 \
  --latency-wait 30 \
  --use-conda \
  --use-singularity \
  -p \
  -R ncbi_download && mv job.* logs/
"""

rule ncbi_download:
    input: 
      'img/genomes_n50.pdf',
      expand("results/mask_check/{spec}_mask_check.tsv", spec = QUERY_GENOMES)

checkpoint species_list:
    output: "results/carnivora_genome_and_timetree.tsv"
    log:
      "logs/r_species_list.log"
    shell: 'Rscript R/compile_species_list.R 2> {log} 1> {log}'

def get_accession(wildcards, what):
    accessions = pd.read_table('data/carnivora_genome_and_timetree.tsv').set_index("spec", drop = False)
    if what == 'name':
        return accessions.loc[wildcards.spec, 'organism_name']
    elif what == 'accession':
        return accessions.loc[wildcards.spec, 'assembly_accession']
    elif what == 'repo':
        return accessions.loc[wildcards.spec, 'repo']

rule geneome_stats:
    output: 'results/genome_stats/{spec}.tsv'
    params:
      name = lambda wc: get_accession(wc, what = "name"),
      accnr = lambda wc: get_accession(wc, what = "accession"),
      repo = lambda wc: get_accession(wc, what = "repo")
    conda:
      'ncbi_datasets'
    shell:
      """
      datasets summary \
        genome accession "{params.accnr}" \
        --assembly-source {params.repo}  \
        --as-json-lines | \
        dataformat tsv \
        genome --fields \
        organism-name,accession,assminfo-name,annotinfo-name,assmstats-scaffold-n50,assmstats-contig-l50,assmstats-total-sequence-len,annotinfo-release-date \
        > {output}
      """

rule download_genome:
    output: 'results/genomes/{spec}/{spec}.zip'
    params:
      name = lambda wc: get_accession(wc, what = "name"),
      accnr = lambda wc: get_accession(wc, what = "accession")
    conda:
      'ncbi_datasets'
    log:
      "logs/genome_dl/genome_dl_{spec}.log"
    shell:
      """
      mkdir -p results/genomes/{wildcards.spec}

      datasets download \
        genome \
        accession {params.accnr} \
        --filename {output} \
        --reference \
        --include genome \
         2> {log} 1> {log}
      """

rule repack_genome:
    input: 
      zp = 'results/genomes/{spec}/{spec}.zip'
    output: 'results/genomes/{spec}/{spec}_org.fa.gz'
    params:
      name = lambda wc: get_accession(wc, what = "name"),
      accnr = lambda wc: get_accession(wc, what = "accession")
    conda:
      "map_align"
    log:
      "logs/genome_uz/genome_uz_{spec}.log"
    shell:
      """
      unzip -p \
        {input.zp} \
        ncbi_dataset/data/{params.accnr}/{params.accnr}*.fna | \
        bgzip > {output}
      """

rule check_if_masked:
    input: 'results/genomes/{spec}/{spec}_org.fa.gz'
    output: 'results/mask_check/{spec}_mask_check.tsv'
    log:
      "logs/mask_check/mask_check_{spec}.log"
    conda:
      "map_align"
    shell:
      """
      # check if grep fails (no match)
      zgrep -v "^>" {input} | grep -q '[atgc]' || GREPERR=$? 
      echo "error code from grep: "$GREPERR

      if [ $GREPERR -eq 1 ]; then 
        echo -e "{wildcards.spec}\t0\tunmasked" > {output}
      else 
        echo -e "{wildcards.spec}\t1\tmasked" > {output}
      fi
      """

rule stat_plots:
    input: 
      stats = expand("results/genome_stats/{spec}.tsv", spec = QUERY_GENOMES),
      genomes = expand("results/genomes/{spec}/{spec}_org.fa.gz", spec = QUERY_GENOMES)
    output: "img/genomes_n50.pdf"
    container: "docker://khench/r_elephant_seal:v0.4"
    log:
      "logs/r_genome_stats.log"
    shell:
      """
      Rscript R/genome_stats.R 2> {log} 1> {log}
      """