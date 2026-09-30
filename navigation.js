// Shared navigation for historical help and our per-version indexes.
const root = new URL('.', document.currentScript.src);
const parts = location.pathname.slice(root.pathname.length).split('/').map(decodeURIComponent);
const version = parts[0];
const isIndex = parts.length === 2;
const header = document.createElement('nav');
header.style.cssText = 'display:flex;flex-wrap:wrap;align-items:center;gap:1em;padding:.6em 0;margin-bottom:1em;border-bottom:1px solid #ccc;font:14px sans-serif';
header.setAttribute('aria-label', 'R help navigation');
header.append('R ' + version + ' · ');
for (const [label, href] of [['All versions', root.href], ['GitHub', 'https://github.com/hughjonesd/r-help']]) {
  const link = document.createElement('a');
  link.textContent = label;
  link.href = href;
  header.append(link);
}
const label = document.createElement('label');
label.append(isIndex ? 'R version ' : 'This topic in ');
const select = document.createElement('select');
select.add(new Option('R ' + version, version, true, true));
label.append(select);
header.append(label);
document.body.prepend(header);

fetch(new URL('versions.txt', root)).then(response => response.text()).then(text => {
  select.replaceChildren();
  for (const other of text.trim().split(/\s+/)) {
    select.add(new Option('R ' + other, other, other === version, other === version));
  }
}).catch(() => { select.disabled = true; });

select.addEventListener('change', async () => {
  const target = new URL(select.value + '/00index.html', root);
  if (!isIndex) {
    try {
      const response = await fetch(new URL(version + '/00index.html', root));
      if (!response.ok) throw new Error('Index unavailable');
      const index = new DOMParser().parseFromString(await response.text(), 'text/html');
      const page = parts.slice(1).join('/');
      const aliases = Array.from(index.querySelectorAll('a[data-name]'))
        .filter(link => decodeURIComponent(link.getAttribute('href')) === page)
        .map(link => link.dataset.name);
      const topic = parts[2].replace(/\.html$/, '');
      target.searchParams.set('package', parts[1]);
      // Prefer the topic's own alias; grouped topics may split in later R.
      for (const name of aliases.includes(topic) ? [topic] : aliases) {
        target.searchParams.append('name', name);
      }
    } catch (error) {
      // The destination index remains useful when a source index is unavailable.
    }
  }
  location.href = target;
});

if (isIndex) {
  const query = new URLSearchParams(location.search);
  const names = query.getAll('name');
  const links = Array.from(document.querySelectorAll('a[data-name]'));
  let matches = links.filter(link => names.includes(link.dataset.name));
  const samePackage = matches.filter(link => link.dataset.package === query.get('package'));
  if (samePackage.length) matches = samePackage;
  const pages = [...new Set(matches.map(link => link.href))];
  if (pages.length === 1) location.replace(pages[0]);
  else if (names.length) {
    const message = document.getElementById('missing');
    message.hidden = false;
    if (pages.length) {
      message.textContent = 'Several help pages match. Select a page: ';
      for (const href of pages) {
        message.append(matches.find(link => link.href === href).cloneNode(true), ' ');
      }
    }
  }
}
