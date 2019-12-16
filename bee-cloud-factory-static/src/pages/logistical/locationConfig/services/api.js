import { api_factory_prefix } from '@/constants/prefix';
export default {
  getList: {
    api: ({ currentPage, pageSize, name }) => `${api_factory_prefix}/configLocation/searchLocationList?currentPage=${currentPage}&pageSize=${pageSize}${name ? `&name=${name}` : ''}`,
    type: 'GET'
  },
  saveLocation: {
    api: () => `${api_factory_prefix}/configLocation/saveLocation`,
    type: `POST`
  },
  updateLocation: {
    api: () => `${api_factory_prefix}/configLocation/updateLocation`,
    type: `POST`
  },
  deleteLocation: {
    api: (id) => `${api_factory_prefix}/configLocation/deleteLocationById/${id}`,
    type: `DELETE`
  }
}