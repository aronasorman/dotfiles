type ServiceList = string[] | null;

export function firstService(services: ServiceList): string | undefined {
  return services.at(0);
}

const discoveredServices: ServiceList = null;
export const selectedService = firstService(discoveredServices);
