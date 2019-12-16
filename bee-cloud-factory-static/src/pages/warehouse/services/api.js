import { api_factory_prefix } from '@/constants/prefix';
export default {
  //仓库档案接口
  warehouseFile: {
    getFiles: {
      api: ({ currentPage, pageSize, name }) => `${api_factory_prefix}/configRepository/searchRepositoryList?currentPage=${currentPage}&pageSize=${pageSize}&name=${name}`, //查询列表
      type: `GET`
    },
    updateFiles: {
      api: () => `${api_factory_prefix}/configRepository/updateRepository`, //修改仓库
      type: `POST`
    },
    addFiles: {
      api: () => `${api_factory_prefix}/configRepository/saveRepository`, //添加仓库
      type: `POST`
    }
  },
  //期初库存接口
  stock: {
    getStocks: {
      api: ({ currentPage, pageSize }) => `${api_factory_prefix}/configOpeningInventoryOrder/searchOpeningInventoryByCondition?currentPage=${currentPage}&pageSize=${pageSize}`, //查询列表
      type: `POST`
    },
    getStockDetail: {
      api: (id) => `${api_factory_prefix}/configOpeningInventoryOrder/getOpeningInventoryById?id=${id}`, //获取期初库存详情
      type: `GET`
    },
    getOrderNumber: {
      api: () => `${api_factory_prefix}/configOpeningInventoryOrder/generateOrderId`, //获取订单编号
      type: `GET`
    },
    getProducts: {
      api: () => `${api_factory_prefix}/configProduct/getProductListByCategory?category=0`, //获取产品
      type: `GET`
    },
    getProductSpec: {
      api: (id) => `${api_factory_prefix}/configProductSpec/getProductSpecByProductId/${id}`, //获取产品规格
      type: `GET`
    },
    getWarehouse: {
      api: () => `${api_factory_prefix}/configRepository/getRepositoryListByType`, //获取仓库列表
      type: `POST`
    },
    saveStock: {
      api: () => `${api_factory_prefix}/configOpeningInventoryOrder/saveOpeningInventoryOrder`, //保存期初库存
      type: `POST`
    }
  }
}
