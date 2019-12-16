import { stringify } from 'querystring';
import request from '@/utils/request';
import { api_factory_prefix } from '@/constants/prefix';

// 合同外磅单列表
export async function getWeightMachineWebBindList(params) {
  return request(`${api_factory_prefix}/weightMachine/getWeightMachineWebBindList?currentPage=${params.currentPage}&pageSize=${params.pageSize}`, {
    method: 'POST',
    body: params,
  });
}

// 关联合同列表
export async function getRelationContract(params) {
  return request(`${api_factory_prefix}/weightMachine/getRelationContract?${stringify(params)}`, {
    method: 'GET',
  });
}

// 磅单关联合同提交
export async function machineBindContract(params) {
  return request(`${api_factory_prefix}/weightMachine/machineBindContract`, {
    method: 'POST',
    body: params,
  });
}

// 磅单忽略
export async function ignoreRecoveryMachine(params) {
  return request(`${api_factory_prefix}/weightMachine/ignoreRecoveryMachine`, {
    method: 'POST',
    body: params,
  });
}
