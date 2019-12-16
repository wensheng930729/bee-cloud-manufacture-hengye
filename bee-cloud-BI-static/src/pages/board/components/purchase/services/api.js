import { api_factory_prefix } from '@/constants/prefix';
export default {
  getPrice: {
    api: () => `${api_factory_prefix}/lookBoard/getPurchaseMoneyRatio`,
    type: 'POST'
  },
  getAmount: {
    api: () => `${api_factory_prefix}/lookBoard/getPurchaseAmountRatio`,
    type: 'POST'
  },
  getPay: {
    api: () => `${api_factory_prefix}/lookBoard/getPurchasePaymentRatio`,
    type: 'POST'
  },
  getRatio: {
    api: () => `${api_factory_prefix}/lookBoard/getPurchasePassRatio`,
    type: 'POST'
  },
  getPosition: {
    api: (type) => `${api_factory_prefix}/lookBoard/getBuyUnfinishedFinance?type=${type}`,
    type: 'GET'
  },
  getGoods: {
    api: (type) => `${api_factory_prefix}/lookBoard/getBuyUnfinishedGoods`,
    type: 'POST'
  }
}