import { api_factory_prefix } from '@/constants/prefix';
export default {
  //查询产品类别下拉列表
  getProductCategoryList: {
    api: () =>
      `${api_factory_prefix}/configProductCategory/getProductCategoryList`,
    type: 'GET'
  },

  //查询产品列表
  searchProductList: {
    api: params =>
      `${api_factory_prefix}/configProduct/searchProductList?${params}`,
    type: 'POST'
  },

  //查询产品详情
  getProductById: {
    api: id =>
      `${api_factory_prefix}/configProduct/getProductById?id=${id}`,
    type: 'GET'
  },

  //保存产品信息
  saveProduct: {
    api: params => `${api_factory_prefix}/configProduct/saveProduct`,
    type: 'POST'
  },

  //修改产品信息
  updateProduct: {
    api: params =>
      `${api_factory_prefix}/configProduct/updateProduct`,
    type: 'POST'
  },

  //查询码表
  listConfigCodeByType: {
    api: params =>
      `${api_factory_prefix}/configCode/listConfigCodeByType?type=${params}`,
    type: 'GET'
  },

  //文件上传
  fileUpload: {
    api: params => `${api_factory_prefix}/file/upload?type=2`,
    type: 'POST'
  },

  //修改化验配置
  updateTestItem: {
    api: params =>
      `${api_factory_prefix}/configProduct/updateTestItem`,
    type: 'POST'
  },

  //修改规格配置
  updateProductSpec: {
    api: params =>
      `${api_factory_prefix}/configProductSpec/updateProductSpec`,
    type: 'POST'
  },

  //修改规格配置
  getProductSpecByProductId: {
    api: id =>
      `${api_factory_prefix}/configProductSpec/getProductSpecByProductId/${id}`,
    type: 'GET'
  },

  //获取产品类别列表
  searchProductCategoryList: {
    api: (a) =>
      `${api_factory_prefix}/configProductCategory/searchProductCategoryList?${a}`,
    type: 'POST'
  },

  //保存产品类别
  saveProductCategory: {
    api: () =>
      `${api_factory_prefix}/configProductCategory/saveProductCategory`,
    type: 'POST'
  },

  //修改产品类别
  updateProductCategory: {
    api: () =>
      `${api_factory_prefix}/configProductCategory/updateProductCategory`,
    type: 'POST'
  },
  //删除产品类别
  deleteProductCategoryById: {
    api: id =>
      `${api_factory_prefix}/configProductCategory/deleteProductCategoryById?id=${id}`,
    type: 'DELETE'
  },

  getTestAttributeByType: {
    api: () => `${api_factory_prefix}/configTestAttribute/getTestAttributeByType`,
    type: `POST`
  },
}
