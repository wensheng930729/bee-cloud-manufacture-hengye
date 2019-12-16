import { api_factory_prefix } from '@/constants/prefix';
export default {
  getBoard: {
    api: () => `${api_factory_prefix}/lookBoard/getDataScreen`,
    type: "GET"
  },
  getConfig: {
    //data_overview：顶部，purchase：采购，produce：生产，sale：销售，stock：库存
    api: (type) => `${api_factory_prefix}/configLookBoardBi/getEnableLookBoardBiList?type=${type}`,
    type: 'GET'
  }
}