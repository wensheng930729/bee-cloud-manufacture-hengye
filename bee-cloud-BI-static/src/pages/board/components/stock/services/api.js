import { api_factory_prefix } from '@/constants/prefix';
export default {
  getStorage: {
    api: ({ dateTime, goodsType }) => `${api_factory_prefix}/storage/selectInStorage?dateTime=${dateTime}&goodsType=${goodsType}`,
    type: "GET"
  },
  getOut: {
    api: ({ startTime, endTime, goodsType, type }) => `${api_factory_prefix}/storage/selectOutStorage?startTime=${startTime}&endTime=${endTime}&goodsType=${goodsType}&type=${type}`,
    type: "GET"
  },
  getOnTheWay: {
    api: (goodsType) => `${api_factory_prefix}/storage/selectInTransit?goodsType=${goodsType}`,
    type: "GET"
  },
}