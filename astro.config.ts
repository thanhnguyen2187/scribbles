import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import { fileURLToPath } from "node:url";
import remarkMath from "remark-math";
import rehypeMathjax from "rehype-mathjax";

// https://astro.build/config
export default defineConfig({
  integrations: [
    starlight({
      title: "Thanh's Scribbles",
      customCss: [
        "./src/styles/custom.css",
      ],
      social: {
        github: "https://github.com/thanhnguyen2187/scribbles",
      },
      sidebar: [
        {
          label: "Miscellanies",
          items: [
            {
              label: "REPL-Driven Development",
              slug: "miscellanies/repl-driven-development",
            },
          ],
        },
        {
          label: "Software Design for Flexibility",
          items: [
            { slug: "sdf/100-index" },
            {
              label: "Chapter 2",
              autogenerate: { directory: "sdf/chapter-2" },
            },
          ],
        },
        {
          label: "Upper and Lower Bounds for Stochastic Processes: Decomposition Theorems",
          items: [
            { slug: "ulbfsp/start-here" },
            { slug: "ulbfsp/terminology" },
            {
              label: "Chapter 1",
              autogenerate: { directory: "ulbfsp/start-here" },
            },
            {
              label: "Chapter 2",
              autogenerate: { directory: "ulbfsp/chapter-2" },
            },
          ],
        },
      ],
    }),
  ],
  markdown: {
    remarkPlugins: [remarkMath],
    rehypePlugins: [rehypeMathjax],
  },
});
