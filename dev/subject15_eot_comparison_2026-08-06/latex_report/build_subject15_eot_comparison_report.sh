#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
analysis_dir="$(cd -- "${script_dir}/.." && pwd)"
repo_root="$(cd -- "${analysis_dir}/../.." && pwd)"
output_dir="${repo_root}/output/pdf"
pdf_name="subject15_eot_basin_comparison_report.pdf"

Rscript "${analysis_dir}/derive_subject15_eot_comparison.R"
Rscript "${script_dir}/build_report_figures.R"

build_datetime="$(TZ=America/New_York date '+%Y-%m-%d %H:%M:%S %Z')"
printf '\\renewcommand{\\reportbuilddatetime}{%s}\n' "${build_datetime}" \
  > "${script_dir}/subject15_eot_comparison_report_build_info.tex"

(
  cd "${script_dir}"
  latexmk -pdf -interaction=nonstopmode -halt-on-error \
    subject15_eot_comparison_report.tex
)

mkdir -p "${output_dir}"
cp \
  "${script_dir}/subject15_eot_comparison_report.pdf" \
  "${output_dir}/${pdf_name}"

printf 'Built %s\n' "${output_dir}/${pdf_name}"
