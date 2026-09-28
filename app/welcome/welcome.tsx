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
          <h1 className="mt-6 text-4xl font-semibold tracking-tight text-stone-900 sm:text-5xl">
            Welcome to my interactive resume app!
          </h1>
          <p className="mt-6 max-w-2xl text-lg leading-8 text-stone-700 sm:text-xl">
            <p>My name is Clio Bate, and I am a GIS Developer with an MS in GIS from Clark University in Worcester, MA, which I earned in May 2024.
            I went directly into my master’s degree after graduating from Smith College with a bachelor’s degree in Environmental Science and Policy,
            where I focused on the human impacts of climate change.</p>
            <p>In my current role as a GIS Developer, I develop Python-based geospatial tools and automate complex GIS workflows using the Esri ecosystem. 
            My work includes developing custom geoprocessing tools, supporting enterprise GIS systems, building end-to-end data-collection and quality-assurance workflows. 
            I enjoy working at the intersection of GIS and software development—taking complicated spatial problems and turning them into reliable, repeatable tools that
            make GIS workflows more efficient.</p>
            <p>I am currently expanding my skillset into web development by building this website as a personal interactive resume application. It is a full-stack application built with React Router and Node.js, using TypeScript, Vite, and Docker.</p>
            <p>Outside of GIS and software development, I enjoy reading—my favorite authors are Robin Hobb and Isabel Allende—crocheting, sewing, traveling, spending time with friends, and hanging out with my cat, Loon.</p>

            <p>Please click on the icons below to contact me via email, or to be taken to by GitHub and LinkedIn profiles.</p>

          </p>
        </header>
      </div>
    </main>
  );
}

