// @ts-check

import { createRequire } from "node:module";
/** @type {import("next").NextConfig} */
export default {
  // Workaround source: https://github.com/vercel/next.js/issues/29362#issuecomment-971377869
  webpack(config, { isServer, dev }) {
    config.experiments = config.experiments ?? {};
    config.experiments = {
      asyncWebAssembly: true,
      layers: true,
    };
    config.experiments.syncWebAssembly = true;
    config.resolve.fallback = config.resolve.fallback ?? {};
    const require = createRequire(import.meta.url);
    config.resolve.fallback.assert = require.resolve("assert/");
    config.module.rules.push({
      test: /\.txt$/i,
      type: "asset/source",
    });

    if (!dev && isServer) {
      config.output.webassemblyModuleFilename = "chunks/[id].wasm";
      config.plugins.push(new WasmChunksFixPlugin());
    }

    return config;
  },
};

// NOTE: https://github.com/vercel/next.js/issues/29362#issuecomment-1325187287
class WasmChunksFixPlugin {
  apply(/** @type any */ compiler) {
    compiler.hooks.thisCompilation.tap(
      "WasmChunksFixPlugin",
      (/** @type any */ compilation) => {
        compilation.hooks.processAssets.tap(
          { name: "WasmChunksFixPlugin" },
          (/** @type any */ assets) =>
            Object.entries(assets).forEach(([pathname, source]) => {
              if (!pathname.match(/\.wasm$/)) return;
              compilation.deleteAsset(pathname);

              const name = pathname.split("/")[1];
              const info = compilation.assetsInfo.get(pathname);
              compilation.emitAsset(name, source, info);
            }),
        );
      },
    );
  }
}
