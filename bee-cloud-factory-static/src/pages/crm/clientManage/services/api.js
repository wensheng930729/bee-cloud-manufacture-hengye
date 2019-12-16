import { api_user_prefix } from '@/constants/prefix';
export default {
  getList: {
    api: ({ currentPage, pageSize }) => `${api_user_prefix}/customer/searchCustomerList?currentPage=${currentPage}&pageSize=${pageSize}`, //查询列表
    type: 'POST'
  },
  getDetail: {
    api: (id) => `${api_user_prefix}/customer/getCustomerById?id=${id}`, //查看详情
    type: 'GET'
  },
  saveClient: {
    api: () => `${api_user_prefix}/customer/saveCustomer`, //新增客户，
    type: `POST`
  },
  updateClient: {
    api: () => `${api_user_prefix}/customer/updateCustomer`, //编辑客户，
    type: 'POST'
  }
}