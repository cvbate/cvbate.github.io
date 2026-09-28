import { type RouteConfig, index, route } from "@react-router/dev/routes";

export default [
  index("routes/home.tsx"),
  route("resume-app", "routes/resume-app.tsx"),
] satisfies RouteConfig;
