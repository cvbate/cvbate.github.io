export function Welcome() {
  return (
    <main className="min-h-screen bg-stone-50 px-6 py-20 text-gray-900">
      <div className="mx-auto max-w-5xl">
        <header className="rounded-3xl border border-stone-200 bg-white/80 p-10 shadow-sm backdrop-blur-sm sm:p-14">
          <div className="flex flex-wrap items-center justify-between gap-4">
            <p className="text-sm font-medium uppercase tracking-[0.2em] text-stone-500">
              Welcome
            </p>
            <nav className="ml-auto flex flex-wrap items-center gap-3">
              <a
                href="/"
                className="rounded-full bg-stone-900 px-4 py-2 text-xs font-medium uppercase tracking-[0.2em] text-white transition hover:bg-stone-700"
              >
                Home
              </a>
              <a
                href="/resume-app"
                className="rounded-full border border-stone-300 bg-stone-100 px-4 py-2 text-xs font-medium uppercase tracking-[0.2em] text-stone-600 transition hover:border-stone-400 hover:bg-stone-200"
              >
                Resume App
              </a>
            </nav>
          </div>
          <h1 className="mt-6 max-w-3xl text-4xl font-semibold tracking-tight text-stone-900 sm:text-5xl">
            Welcome to my interactive resume app!
          </h1>

          <div className="mt-6">
            <div className="float-right ml-6 mb-4 w-full max-w-[220px] overflow-hidden rounded-2xl border border-stone-200 bg-stone-100 shadow-sm">
              <img
                src="/clio_bate_profile.jpg"
                alt="Clio Bate"
                className="h-[260px] w-full object-cover"
              />
            </div>

            <div className="space-y-5 text-lg leading-8 text-stone-700 sm:text-xl">
              <p>
                My name is Clio Bate, and I am a GIS Developer with an MS in GIS from Clark University in Worcester, MA, which I earned in May 2024. I went directly into my master’s degree after graduating from Smith College with a bachelor’s degree in Environmental Science and Policy, where I focused on the human impacts of climate change.
              </p>
              <p>
                In my current role as a GIS Developer, I develop Python-based geospatial tools and automate complex GIS workflows using the Esri ecosystem. My work includes developing custom geoprocessing tools, supporting enterprise GIS systems, building end-to-end data-collection and quality-assurance workflows. I enjoy working at the intersection of GIS and software development—taking complicated spatial problems and turning them into reliable, repeatable tools that make GIS workflows more efficient.
              </p>
              <p>
                I am currently expanding my skillset into web development by building this website as a personal interactive resume application. It is a full-stack application built with React Router and Node.js, using TypeScript, Vite, and Docker.
              </p>
              <p>
                Outside of GIS and software development, I enjoy reading—my favorite authors are Robin Hobb and Isabel Allende—crocheting, sewing, traveling, spending time with friends, and hanging out with my cats, Loon and Ptolemy.
              </p>
              <p>
                Please click on the icons below to contact me via email, or to be taken to by GitHub and LinkedIn profiles.
              </p>
            </div>
          </div>

          <footer className="mt-8 flex items-center gap-4 border-t border-stone-200 pt-6">
            <a
              href="mailto:cvalentinebate@gmail.com"
              target="_blank"
              rel="noreferrer"
              aria-label="Email Clio"
              className="flex h-10 w-10 items-center justify-center rounded-full border border-stone-300 bg-stone-100 text-stone-700 transition hover:border-stone-400 hover:bg-stone-200 hover:text-stone-900"
            >
              <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.8" className="h-5 w-5">
                <path d="M4 7.5A2.5 2.5 0 0 1 6.5 5h11A2.5 2.5 0 0 1 20 7.5v9A2.5 2.5 0 0 1 17.5 19h-11A2.5 2.5 0 0 1 4 16.5v-9Z" />
                <path d="m5 7 7 5 7-5" />
              </svg>
            </a>

            <a
              href="https://www.linkedin.com/in/cliovb/"
              target="_blank"
              rel="noreferrer"
              aria-label="Visit LinkedIn"
              className="flex h-10 w-10 items-center justify-center rounded-full border border-stone-300 bg-stone-100 text-stone-700 transition hover:border-stone-400 hover:bg-stone-200 hover:text-stone-900"
            >
              <svg viewBox="0 0 24 24" fill="currentColor" className="h-5 w-5">
                <path d="M6.94 8.5A1.56 1.56 0 1 1 6.94 5.4a1.56 1.56 0 0 1 0 3.1ZM5.5 9.8h2.9V18H5.5V9.8Zm5.2 0h2.8v1.12h.04c.39-.74 1.35-1.52 2.78-1.52 2.97 0 3.52 1.95 3.52 4.49V18h-2.9v-16c0-3.78-4.4-3.18-4.4 0V18H10.7V9.8Z" />
              </svg>
            </a>
          </footer>
        </header>
      </div>
    </main>
  );
}

