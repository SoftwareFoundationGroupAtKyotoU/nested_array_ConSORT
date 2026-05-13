# DARTS Submission TODO

Observations from inspecting `/Users/ksuenaga/work/nested_array_ConSORT/darts`
against the DARTS / ECOOP 2026 author instructions.

## Required Before Submission

- [x] Refresh or verify the official DARTS template bundle.
  - Official DARTS page lists `darts-v2021 v2021.1.3`.
  - Official bundle inspected at `/Users/ksuenaga/Downloads/darts-authors-v2021.1.3`.
  - Local `darts-v2021.cls`, `darts-logo-bw.pdf`, and `cc-by.pdf` are byte-for-byte identical to the official bundle.
  - Official bundle also contains sample files, `LICENSE.md`, `CHANGELOG.md`, and `orcid.pdf`.
  - `orcid.pdf` was copied locally because `darts-v2021.cls` uses it if present.
  - The official class does not define `\mdsum` or `\artifactsize`; the official sample `.tex` defines them in the preamble.
  - The two helper macro definitions were copied into `darts-artifact.tex`; actual checksum/size values still need to be inserted once the final artifact archive is decided.

- [x] Add volume metadata macros.
  - Added `\Volume{12}`, `\Issue{1}`, and `\Article{23}` to `darts-artifact.tex`.
  - The rebuilt PDF renders DOI `10.4230/DARTS.12.1.23` and `Dagstuhl Artifacts Series, Vol. 12, Issue 1, Artifact No. 23`.
  - Publisher-provided snippet contains `\RelatedConference`, `\RelatedArticle`, and `\Article{23}`, and those are reflected in `darts-artifact.tex`.
  - `\Volume{12}` and `\Issue{1}` came from the DARTS Volume 12, Issue 1 instruction.

- [ ] Replace related-paper placeholders.
  - `darts-artifact.tex` intentionally keeps unsure related-paper fields as placeholders:
    - `pp.~[from]--[to]`
    - `https://doi.org/[doi-of-conference-paper]`
  - Keep these placeholders until Dagstuhl/DROPS or the publisher confirms the final LIPIcs related-paper DOI and page/article range.
  - Local main-paper files suggest possible values, but they are not authoritative enough for the DARTS final metadata:
    - `/Users/ksuenaga/work/ECOOP2026/main.tex` has `\SeriesVolume{372}` and `\ArticleNo{15}`.
    - `/Users/ksuenaga/work/ECOOP2026/main.pdf` renders DOI `10.4230/LIPIcs.ECOOP.2026.15`.
    - That local PDF has 31 pages and renders `pp. 15:1--15:31`.
  - As of the check on 2026-05-13, I could not find a public DROPS/DOI record for this exact paper.

- [x] Add artifact MD5 and artifact size.
  - The submitted artifact is the Zenodo record `https://doi.org/10.5281/zenodo.19521221`.
  - `darts-artifact.tex` now includes file-level MD5 sums from the Zenodo API:
    - `nested-array-consort-ecoop26.x86.tar.gz`: `04d2d06f981e876e44a9f4815015f86e`
    - `nested-array-consort-ecoop26.arm64.tar.gz`: `43b0b114ef7b0e475d98337274426571`
    - `nested-array-consort-source.zip`: `dfc7f63fa49aa9921bff7603b4394147`
    - `ARTIFACT.md`: `3a5898add4067a10925b993dc586a333`
    - `LICENSE`: `817c302b311b2996450510af7cb0990a`
  - `darts-artifact.tex` now lists total Zenodo artifact payload size as `13,129,518,292` bytes, approximately `12.23 GiB`.

- [x] Decide the actual submitted artifact file.
  - Use the Zenodo record as the artifact: `https://doi.org/10.5281/zenodo.19521221`.
  - The manuscript now has an `Artifact Archive` section pointing to this DOI.
  - Do not include artifact files in the LaTeX-source zip.

- [x] Resolve architecture consistency.
  - Zenodo record contains both prebuilt Docker images:
    - `nested-array-consort-ecoop26.x86.tar.gz`
    - `nested-array-consort-ecoop26.arm64.tar.gz`
  - The manuscript now gives separate `docker load` commands for x86_64 and ARM64.

- [ ] Add explicit origin/version information.
  - Description should identify:
    - GitHub repository: `https://github.com/SoftwareFoundationGroupAtKyotoU/nested_array_ConSORT`
    - final commit hash, tag, or release version
    - Zenodo DOI or artifact archive DOI if used
  - Current branch: `ecoop26-artifact-eval`
  - Current HEAD when inspected: `2aff3df9fb5b541dc5d33e245b08f456dd42aa31`
  - No git tags were present when inspected.
  - Working tree was dirty, so do not cite the above hash as final until changes are committed.

- [ ] Resolve Zenodo license metadata.
  - The manuscript now states that the software artifact includes a `LICENSE` file for Apache License 2.0, and that the DARTS artifact description is CC BY 4.0.
  - The Zenodo API currently reports record license metadata as `cc-by-4.0`.
  - If the software artifact should be Apache-2.0, update the Zenodo record/license metadata or create a corrected Zenodo version before final submission.

- [x] Fix author affiliations.
  - Author affiliations now match the main paper: `Kyoto University, Kyoto, Japan`.
  - Author email addresses and author-specific funding were also synchronized with the main paper.

- [ ] Polish keywords and bibliography.
  - Dagstuhl FAQ says keywords should be comma-delimited and the first word / proper nouns capitalized.
  - Keywords were synchronized with the main paper: `aliasing, fractional ownership, program verification, refinement types, type systems`.
  - `references.bib` exists, which is good.
  - Add DOI/URL for the Z3 reference if possible.
  - Keep `\bibliographystyle{plainurl}` and submit the `.bib`, not only `.bbl`.

- [x] Update build dependencies.
  - `Makefile` now depends on `darts-v2021.cls`, `darts-logo-bw.pdf`, `cc-by.pdf`, and `orcid.pdf`.

- [ ] Rebuild and clean generated files.
  - Rebuild `darts-artifact.pdf` after metadata is finalized.
  - Generated files in this directory include `.aux`, `.bbl`, `.blg`, `.log`, `.out`, `.pdf`, `.vtc`.
  - Source submission should include the required LaTeX sources/assets and bibliography, not stale generated artifacts unless Dagstuhl explicitly asks for them.

- [ ] Prepare separate submission payloads.
  - LaTeX artifact-description source zip: `.tex`, `.bib`, class/style/assets, figures if any.
  - Artifact upload/link: the actual artifact archive, separate from LaTeX sources.
  - Signed author agreement: download, sign, and upload after LaTeX upload.

## Current Local State When Inspected

- DARTS source file: `darts-artifact.tex`
- Built PDF exists: `darts-artifact.pdf`
- Bibliography exists: `references.bib`
- DARTS class/assets exist: `darts-v2021.cls`, `darts-logo-bw.pdf`, `cc-by.pdf`
- `darts-artifact.tex` already uses `\documentclass{darts-v2021}`.
- Mandatory present metadata:
  - `\title`
  - `\author`
  - `\authorrunning`
  - `\Copyright`
  - `\keywords`
  - `\ccsdesc`
  - `abstract`
- Missing or unresolved metadata:
  - final related-paper DOI
  - final related-paper page/article range
  - Zenodo license metadata if Apache-2.0 is intended for the software artifact
  - final artifact version/tag/commit

## Useful Commands

```sh
md5 ../artifact-package/nested-array-consort-ecoop26.arm64.tar.gz
stat -f "%N %z bytes" ../artifact-package/nested-array-consort-ecoop26.arm64.tar.gz
curl -L https://zenodo.org/api/records/19521221
make
```
