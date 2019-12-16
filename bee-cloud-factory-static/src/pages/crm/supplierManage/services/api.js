import { api_user_prefix } from '@/constants/prefix';
export default {
  getList: {
    api: ({ currentPage, pageSize }) => `${api_user_prefix}/supplier/searchSupplierList?currentPage=${currentPage}&pageSize=${pageSize}`, //查询列表
    type: 'POST'
  },
  getDetail: {
    api: (id) => `${api_user_prefix}/supplier/getSupplierById?id=${id}`, //查看详情
    type: 'GET'
  },
  saveSupplier: {
    api: () => `${api_user_prefix}/supplier/saveSupplier`, //新增供应商，
    type: `POST`
  },
  updateSupplier: {
    api: () => `${api_user_prefix}/supplier/updateSupplier`, //编辑供应商，
    type: 'POST'
  }
}