import { api_factory_prefix } from '@/constants/prefix';
export default {
  getList: {
    api: ({ currentPage, pageSize, name }) => `${api_factory_prefix}/configLookBoardBi/searchLookBoardBiList?currentPage=${currentPage}&pageSize=${pageSize}${name ? `&name=${name}` : ''}`,
    type: 'GET'
  },
  enable: {
    api:() => `${api_factory_prefix}/configLookBoardBi/enableLookBoardBi`,
    type: `POST`
  },
  disable: {
    api:() => `${api_factory_prefix}/configLookBoardBi/stopLookBoardBi`,
    type: 'POST'
  }
}
