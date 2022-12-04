import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
    root: "src",
    build: { outDir: "dist" },
    plugins: [elmPlugin()],
});