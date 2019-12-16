import { api_factory_prefix } from '@/constants/prefix';
export default {
  getList: {
    api: ({ currentPage, pageSize, productName }) => `${api_factory_prefix}/configRawMaterialLoss/searchRawMaterialLossList?currentPage=${currentPage}&pageSize=${pageSize}${productName ? `&productName=${productName}` : ''}`,
    type: 'GET'
  },
  getDetail: {
    api: (id) => `${api_factory_prefix}/configRawMaterialLoss/getRawMaterialLossById/${id}`,
    type: 'GET'
  },
  getProType: {
    api: () => `${api_factory_prefix}/configProduct/getProductListByCategory?category=5`, //获取产品类别
    type: `GET`
  },
  saveLoss: {
    api: () => `${api_factory_prefix}/configRawMaterialLoss/saveRawMaterialLoss`,
    type: `POST`
  },
  updateLoss: {
    api: () => `${api_factory_prefix}/configRawMaterialLoss/updateRawMaterialLoss`,
    type: `POST`
  },
  deleteLoss: {
    api: (id) => `${api_factory_prefix}/configRawMaterialLoss/deleteRawMaterialLossById/${id}`,
    type: 'DELETE'
  }
}