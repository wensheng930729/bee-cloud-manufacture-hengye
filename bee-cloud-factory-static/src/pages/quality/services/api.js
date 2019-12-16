import { api_factory_prefix } from '@/constants/prefix';
export default {
  //获取化验属性列表
  searchTestAttributeList: {
    api: params =>
      `${api_factory_prefix}/configTestAttribute/searchTestAttributeList?${params}`,
    type: 'GET'
  },

  //保存化验属性
  saveTestAttribute: {
    api: params =>
      `${api_factory_prefix}/configTestAttribute/saveTestAttribute`,
    type: 'POST'
  },

  //修改化验属性
  updateTestAttribute: {
    api: params =>
      `${api_factory_prefix}/configTestAttribute/updateTestAttribute`,
    type: 'POST'
  },

  //删除化验属性
  deleteTestAttributeById: {
    api: id =>
      `${api_factory_prefix}/configTestAttribute/deleteTestAttributeById/${id}`,
    type: 'DELETE'
  }
}
