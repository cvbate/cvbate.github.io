import type { Route } from "./+types/resume-app";

export function meta({}: Route.MetaArgs) {
  return [
    { title: "Resume App | Clio Bate" },
    { name: "description", content: "Resume app page for Clio Bate." },
  ];
}

export default function ResumeApp() {
  return (
    <main className="min-h-screen bg-stone-50 px-6 py-20 text-gray-900">
      <div className="mx-auto max-w-5xl">
        <header className="rounded-3xl border border-stone-200 bg-white/80 p-10 shadow-sm backdrop-blur-sm sm:p-14">
          <div className="flex flex-wrap items-center justify-between gap-4">
            <p className="text-sm font-medium uppercase tracking-[0.2em] text-stone-500">
              Resume App
            </p>
            <nav className="ml-auto flex flex-wrap items-center gap-3">
              <a
                href="/"
                className="rounded-full border border-stone-300 bg-stone-100 px-4 py-2 text-xs font-medium uppercase tracking-[0.2em] text-stone-600 transition hover:border-stone-400 hover:bg-stone-200"
              >
                Home
              </a>
              <a
                href="/resume-app"
                className="rounded-full bg-stone-900 px-4 py-2 text-xs font-medium uppercase tracking-[0.2em] text-white transition hover:bg-stone-700"
              >
                Resume App
              </a>
            </nav>
          </div>

          <h1 className="mt-6 text-4xl font-semibold tracking-tight text-stone-900 sm:text-5xl">
            Resume details
          </h1>
          <p className="mt-6 max-w-2xl text-lg leading-8 text-stone-700 sm:text-xl">
            This page is ready for your experience, projects, and contact information.
          </p>
        </header>
      </div>
    </main>
  );
}
