import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import { fileURLToPath } from "node:url";

// https://astro.build/config
export default defineConfig({
  integrations: [
    starlight({
      title: "Thanh's Scribbles",
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
      ],
    }),
  ],
});
