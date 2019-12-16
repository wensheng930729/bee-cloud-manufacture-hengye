import { api_factory_prefix } from '@/constants/prefix';
export default {
  getPrice: {
    api: () => `${api_factory_prefix}/lookBoard/getSaleMoneyRatio`,
    type: 'POST'
  },
  getAmount: {
    api: () => `${api_factory_prefix}/lookBoard/getSaleAmountRatio`,
    type: 'POST'
  },
  getPay: {
    api: () => `${api_factory_prefix}/lookBoard/getSaleMoneyBackRatio`,
    type: 'POST'
  },
  getRatio: {
    api: () => `${api_factory_prefix}/lookBoard/getSalePassRatio`,
    type: 'POST'
  },
  getPosition: {
    api: (type) => `${api_factory_prefix}/lookBoard/getSaleUnfinishedFinance?type=${type}`,
    type: 'GET'
  },
  getGoods: {
    api: (type) => `${api_factory_prefix}/lookBoard/getSaleUnfinishedGoods`,
    type: 'POST'
  }
}