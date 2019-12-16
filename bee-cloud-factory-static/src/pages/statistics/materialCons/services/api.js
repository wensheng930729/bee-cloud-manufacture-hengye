import { api_factory_prefix } from '@/constants/prefix';
export default {
  //条件查询原料吨耗列表
  searchMaterialsConsumptionList: {
    api: params =>
      `${api_factory_prefix}/configMaterialsConsumption/searchMaterialsConsumptionList?${params}`,
    type: 'GET'
  },

  //根据类型查询产品列表
  getProductListByCategory: {
    api: id =>
      `${api_factory_prefix}/configProduct/getProductListByCategory?category=${id}`,
    type: 'GET'
  },

  //保存原料吨耗
  saveMaterialsConsumption: {
    api: id =>
      `${api_factory_prefix}/configMaterialsConsumption/saveMaterialsConsumption`,
    type: 'POST'
  },

  //修改原料吨耗
  updateMaterialsConsumption: {
    api: id =>
      `${api_factory_prefix}/configMaterialsConsumption/updateMaterialsConsumption`,
    type: 'POST'
  },

  //删除原料吨耗
  deleteMaterialsConsumptionById: {
    api: id =>
      `${api_factory_prefix}/configMaterialsConsumption/deleteMaterialsConsumptionById?id=${id}`,
    type: 'DELETE'
  },

  //查询原料吨耗详情
  getMaterialsConsumptionById: {
    api: id =>
      `${api_factory_prefix}/configMaterialsConsumption/getMaterialsConsumptionById?id=${id}`,
    type: 'GET'
  }
}
