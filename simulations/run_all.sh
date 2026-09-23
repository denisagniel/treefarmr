#!/bin/bash
# run_all.sh — Submit all optimaltrees simulation jobs to SLURM (O2 cluster)
#
# Usage:
#   bash run_all.sh [--dry-run] [--jobs C1,C2,C3,C4]
#
# Options:
#   --dry-run     Print sbatch commands without submitting
#   --jobs        Comma-separated subset of jobs to run (default: all)
#                 Valid values: C1, C2, C3, C4
#
# Module versions: gcc/14.2.0, R/4.4.2
# Authoritative source: .claude/skills/setup-cluster-simulations/SKILL.md
#
# Job overview:
#   C1: Rate estimation     — 2 DGPs x 7 n-values x 10 batches = 140 array tasks
#   C2: High-dim stress     — 2 DGPs x 2 n x 4 p x 5 batches  =  80 array tasks
#   C3: Overlap stress      — 4 n-values x 10 batches           =  40 array tasks
#   C4: CV consistency      — 2 DGPs x 6 n-values x 5 batches  =  60 array tasks
#
# Expected runtimes (O2 short partition, 8G, 1h limit):
#   C1: 140 jobs x ~20 min (n=50k jobs) -> run in batches if needed
#   C2: 80 jobs x ~30 min (p=20 jobs)
#   C3: 40 jobs x ~15 min
#   C4: 60 jobs x ~30 min (n=10k CV)
#
# Output: results saved to $SCRATCH_DIR/{rate,highdim,overlap,cv}/
# After completion: run Rscript combine_results.R to aggregate

set -euo pipefail

# ---- Configuration -----------------------------------------------------------

# Cluster paths — edit these for your setup
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRATCH_BASE="/n/scratch/users/$(echo "$USER" | cut -c1)/${USER}/optimaltrees_sims"
LOG_DIR="${SCRATCH_BASE}/logs"
REPS_PER_JOB=10  # replications per SLURM array task

# O2 module versions — authoritative: .claude/skills/setup-cluster-simulations/SKILL.md
GCC_VERSION="gcc/14.2.0"
R_VERSION="R/4.4.2"

# Parse arguments
DRY_RUN=false
JOBS_TO_RUN="C1,C2,C3,C4"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run)  DRY_RUN=true; shift ;;
    --jobs)     JOBS_TO_RUN="$2"; shift 2 ;;
    *) echo "Unknown argument: $1"; exit 1 ;;
  esac
done

# ---- Setup -------------------------------------------------------------------

mkdir -p "${LOG_DIR}"
echo "================================================================"
echo "optimaltrees simulation study — SLURM submission"
echo "Date: $(date)"
echo "Script dir: ${SCRIPT_DIR}"
echo "Scratch dir: ${SCRATCH_BASE}"
echo "Reps per job: ${REPS_PER_JOB}"
echo "Jobs to run: ${JOBS_TO_RUN}"
echo "Dry run: ${DRY_RUN}"
echo "================================================================"

# Submit function
submit_job() {
  local job_name="$1"
  local array_size="$2"
  local r_script="$3"
  local out_dir="$4"
  local time_limit="${5:-01:00:00}"
  local mem="${6:-8G}"

  echo ""
  echo "--- Submitting ${job_name} (array 1-${array_size}) ---"

  local cmd="sbatch \
    --job-name=${job_name} \
    --array=1-${array_size} \
    --time=${time_limit} \
    --mem=${mem} \
    --partition=short \
    --output=${LOG_DIR}/${job_name}_%A_%a.out \
    --error=${LOG_DIR}/${job_name}_%A_%a.err \
    --wrap=\"\
      module load ${GCC_VERSION} ${R_VERSION} && \
      mkdir -p ${out_dir} && \
      Rscript ${SCRIPT_DIR}/${r_script} \
        --job_id \$SLURM_ARRAY_TASK_ID \
        --n_reps ${REPS_PER_JOB} \
        --out_dir ${out_dir}\""

  echo "Command: ${cmd}"
  if [ "${DRY_RUN}" = false ]; then
    eval "${cmd}"
    echo "Submitted."
  else
    echo "[DRY RUN — not submitted]"
  fi
}

# ---- Job C1: Rate estimation (RMSE vs n at n up to 50k) ----------------------
# 2 DGPs x 7 n-values = 14 configs; 100 reps / 10 per job = 10 batches
# Total array size: 14 * 10 = 140

if echo "${JOBS_TO_RUN}" | grep -q "C1"; then
  N_C1_CONFIGS=14
  N_C1_BATCHES=10
  C1_ARRAY=$((N_C1_CONFIGS * N_C1_BATCHES))

  # n=50k jobs may need longer — submit separately if needed
  # For n <= 8k jobs: 15-20 min. For n=20k-50k: 30-60 min.
  submit_job "ot_rate" "${C1_ARRAY}" \
    "sim_rate_estimation.R" \
    "${SCRATCH_BASE}/rate" \
    "01:30:00" "8G"
fi

# ---- Job C2: High-dim stress (p up to 20) ------------------------------------
# 2 DGPs x 2 n x 4 p = 16 configs; 50 reps / 10 per job = 5 batches
# Total array size: 16 * 5 = 80

if echo "${JOBS_TO_RUN}" | grep -q "C2"; then
  N_C2_CONFIGS=16
  N_C2_BATCHES=5
  C2_ARRAY=$((N_C2_CONFIGS * N_C2_BATCHES))

  # p=20 jobs may need more memory (K^p feature combinations)
  submit_job "ot_highdim" "${C2_ARRAY}" \
    "sim_highdim_stress.R" \
    "${SCRATCH_BASE}/highdim" \
    "01:00:00" "12G"
fi

# ---- Job C3: Overlap stress (propensity near 0/1) ---------------------------
# 4 n-values; 100 reps / 10 per job = 10 batches
# Total array size: 4 * 10 = 40

if echo "${JOBS_TO_RUN}" | grep -q "C3"; then
  N_C3_CONFIGS=4
  N_C3_BATCHES=10
  C3_ARRAY=$((N_C3_CONFIGS * N_C3_BATCHES))

  submit_job "ot_overlap" "${C3_ARRAY}" \
    "sim_propensity_overlap.R" \
    "${SCRATCH_BASE}/overlap" \
    "01:00:00" "8G"
fi

# ---- Job C4: CV consistency (lambda scale check) ----------------------------
# 2 DGPs x 6 n-values = 12 configs; 50 reps / 10 per job = 5 batches
# Total array size: 12 * 5 = 60

if echo "${JOBS_TO_RUN}" | grep -q "C4"; then
  N_C4_CONFIGS=12
  N_C4_BATCHES=5
  C4_ARRAY=$((N_C4_CONFIGS * N_C4_BATCHES))

  # CV at n=10k is slow (many folds)
  submit_job "ot_cv" "${C4_ARRAY}" \
    "sim_cv_consistency.R" \
    "${SCRATCH_BASE}/cv" \
    "01:30:00" "8G"
fi

echo ""
echo "================================================================"
echo "Submission complete."
echo "Monitor with: squeue -u \$USER"
echo "After completion: Rscript ${SCRIPT_DIR}/combine_results.R \\"
echo "  --scratch_dir ${SCRATCH_BASE}"
echo "================================================================"
