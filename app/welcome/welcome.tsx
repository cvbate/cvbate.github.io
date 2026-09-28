export function Welcome() {
  return (
    <main className="min-h-screen bg-stone-50 px-6 py-20 text-gray-900">
      <div className="mx-auto max-w-4xl">
        <header className="rounded-3xl border border-stone-200 bg-white/80 p-10 shadow-sm backdrop-blur-sm sm:p-14">
          <p className="mb-4 text-sm font-medium uppercase tracking-[0.2em] text-stone-500">
            Welcome
          </p>
          <h1 className="text-4xl font-semibold tracking-tight text-stone-900 sm:text-5xl">
            Welcome to my interactive resume app!
          </h1>
          <p className="mt-6 max-w-2xl text-lg leading-8 text-stone-700 sm:text-xl">
            My name is <span className="font-semibold text-stone-900">Clio Bate</span>,
            and I am a <span className="font-semibold text-stone-900">GIS Developer </span>
            based in <span className="font-semibold text-stone-900">Pittsburgh, PA</span>.
          </p>
        </header>
      </div>
    </main>
  );
}

