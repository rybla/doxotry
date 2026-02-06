import fs from "fs/promises";
import path from "path"

// -----------------------------------------------------------------------------

const pages_dirpath = "page"
const dist_dirpath = "dist"

// -----------------------------------------------------------------------------

const page_filepaths =
    (await fs.readdir(pages_dirpath))
        .map((filename) => path.join(pages_dirpath, filename))

await fs.rm(dist_dirpath, { recursive: true, force: true });

await Bun.build({
    entrypoints: page_filepaths,
    outdir: dist_dirpath,
    minify: false,
})

