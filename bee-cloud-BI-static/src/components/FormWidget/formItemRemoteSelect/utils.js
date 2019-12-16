export const dealSearchEntityWithWrite = obj => {
  const { data } = obj;

  const newData = data.map(item => ({
    value: item.id,
    label: item.name,
    origin: '',
  }));
  return newData;
};
