import { api_user_prefix } from '@/constants/prefix';
export default {
  getList: {
    api: ({ currentPage, pageSize }) => `${api_user_prefix}/authCustomerOrSupplierAccount/searchCustomerOrSupplierList?currentPage=${currentPage}&pageSize=${pageSize}`, //查询列表
    type: 'POST'
  },
  getTypeList: {
    api: ({ currentPage, pageSize }) => `${api_user_prefix}/authCustomerOrSupplierAccount/searchAccountList?currentPage=${currentPage}&pageSize=${pageSize}`, //查询账户列表
    type: 'POST'
  },
  getDetail: {
    api: (id) => `${api_user_prefix}/authCustomerOrSupplierAccount/getAccountById?id=${id}`, //查看详情
    type: 'GET'
  },
  saveAccount: {
    api: () => `${api_user_prefix}/authCustomerOrSupplierAccount/saveAccount`, //新增，
    type: 'POST'
  },
  updateAccount: {
    api: () => `${api_user_prefix}/authCustomerOrSupplierAccount/updateAccount`, //编辑，
    type: `POST`
  },
  changePassword: {
    api: () => `${api_user_prefix}/authCustomerOrSupplierAccount/updateUserPassword`,
    type: `POST`
  }
}