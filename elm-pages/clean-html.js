const { promises: fs } = require("fs");
const cheerio = require('cheerio');

const file = process.argv[2];

(async () => {
    const html = await fs.readFile(file,"utf8");
    const $ = cheerio.load(html);

    $('script').remove();
    $('link[rel=preload]').remove();
    $('link[rel=modulepreload]').remove();

    await fs.writeFile(file, $.html(),"utf8");
})();
