import React, { Component } from 'react';
import PropTypes from 'prop-types';
import moment from 'moment';
import { uniqBy } from 'lodash';
import { Card, Button, message, Form } from 'antd';
import {
  StandardTable,
  FormItemInputSearch,
  FormItemSelect,
  CustomFormHOC,
  FormItemRangePicker,
} from '@/components/FormWidget';

import { getWeighListInfo } from '../services';

import styles from '../index.less';

const weightTypeOptions = [
  { value: '1', label: '采购' },
  { value: '2', label: '销售' },
];

const MyFormSearch = props => {
  const {
    form: { getFieldDecorator },
    onSearch,
  } = props;
  const formItemLayout = {};

  return (
    <Form layout="inline">
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label="合同编号"
        fieldId="contractNum"
        required={false}
        formItemLayout={formItemLayout}
      />
      <FormItemInputSearch
        getFieldDecorator={getFieldDecorator}
        label="车牌号"
        fieldId="trainNumber"
        required={false}
        formItemLayout={formItemLayout}
      />

      <FormItemSelect
        getFieldDecorator={getFieldDecorator}
        label="称重类型"
        fieldId="weighingType"
        required={false}
        selectProps={{ options: weightTypeOptions }}
        formItemLayout={formItemLayout}
      />
      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label="过磅日期"
        fieldId="dateRange"
        required={false}
        formItemLayout={formItemLayout}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
      />
      <Form.Item {...formItemLayout}>
        <Button type="primary" onClick={onSearch}>
          查询
        </Button>
      </Form.Item>
    </Form>
  );
};

const NewFormSearch = CustomFormHOC(MyFormSearch);

class Index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: false,
      data: { list: [], pagination: {} },

      searchValue: {
        contractNum: '', // 合同编号
        trainNumber: '',
        weighingType: '1',
        dateRange: {
          startTime: moment()
            .subtract(30, 'days')
            .format('YYYY-MM-DD'),
          endTime: moment().format('YYYY-MM-DD'),
        },
        currentPage: 1,
        orderStage: '',
        pageSize: 10,
      },
      // "ascend"  "descend"
      sortOrder: 'descend',
    };
  }

  componentDidMount() {
    try {
      _czc1.push(['_trackEvent', '地磅单', '查看', '', '', '']);
    } catch (error) {}
    this.getTableData();
  }

  // 搜索按钮 点击事件
  handleSearch = () => {
    this.getTableData();
  };

  // 表单查询改变事件
  handleFormChange = field => {
    this.getTableData(field);
  };

  // 表格分页事件
  handleTableChange = (pagination, filters, sorter) => {
    const { current, pageSize } = pagination;
    const { searchValue, sortOrder } = this.state;

    this.setState({
      sortOrder: sorter.order,
    });
    if (
      (searchValue.currentPage === current && searchValue.pageSize === pageSize) ||
      sorter.order !== sortOrder
    ) {
      return;
    }

    this.getTableData({ currentPage: current, pageSize });
  };

  // 获取表格数据
  getTableData = (obj = {}) => {
    this.setState({
      loading: true,
    });
    const { searchValue } = this.state;

    const newsearchValue = { ...searchValue, ...obj };

    const {
      dateRange: { startTime, endTime },
      ...rest
    } = newsearchValue;

    const params = { startTime, endTime, ...rest };

    getWeighListInfo(params).then(res => {
      if (res.code === 1) {
        const list = res.object;
        if (list.length > 0) {
          const { currentPage, pageSize, searchCount, totalPage, totalRecords } = res.page;
          const pagination = {
            pageSize,
            current: currentPage,
            total: totalRecords,
          };
          this.setState({
            data: { list: uniqBy(list, 'machineId'), pagination },
          });
        } else {
          this.setState({
            data: { list: [], pagination: { pageSize: 10, current: 1, total: 0 } },
          });
        }

        this.setState({
          loading: false,
        });
      } else {
        message.error(res.message);
        this.setState({
          loading: false,
        });
      }
    });

    this.setState({
      searchValue: newsearchValue,
    });
  };

  render() {
    const { loading, data, searchValue } = this.state;

    const searchProps = {
      dataDetail: searchValue,
      onChange: this.handleFormChange,
      onSearch: this.handleSearch,
    };

    const columns = [
      {
        title: '磅单号',
        dataIndex: 'machineId',
        key: 'machineId',
      },
      {
        title: '合同编号',
        dataIndex: 'contractNum',
        key: 'contractNum',
      },
      {
        title: '客户/供应商',
        dataIndex: 'merchants',
        key: 'merchants',
      },
      {
        title: '车号',
        dataIndex: 'trainNumber',
        key: 'trainNumber',
      },
      {
        title: '毛重',
        dataIndex: 'grossWeight',
        key: 'grossWeight',
      },
      {
        title: '皮重',
        dataIndex: 'tareWeight',
        key: 'tareWeight',
      },
      {
        title: '扣重',
        dataIndex: 'deductWeight',
        key: 'deductWeight',
      },
      {
        title: '净重',
        dataIndex: 'netWeight',
        key: 'netWeight',
      },

      // {
      //   title: '产品名称',
      //   dataIndex: 'productName',
      //   key: 'productName',
      // },
      // {
      //   title: '承运商',
      //   dataIndex: 'carrierName',
      //   key: 'carrierName',
      // },

      {
        title: '司磅员',
        dataIndex: 'weighingMan',
        key: 'weighingMan',
      },
      {
        title: '称重日期',
        dataIndex: 'weighingTime',
        key: 'weighingTime',
        sortOrder: this.state.sortOrder,
        sorter: (a, b) => {
          const yearA = a.weighingTime.slice(0, 4);
          const monthA = a.weighingTime.slice(5, 7);
          const dayA = a.weighingTime.slice(8, 10);

          const newAStr = `${yearA}-${monthA}-${dayA}`;

          const yearB = b.weighingTime.slice(0, 4);
          const monthB = b.weighingTime.slice(5, 7);
          const dayB = b.weighingTime.slice(8, 10);

          const newBStr = `${yearB}-${monthB}-${dayB}`;

          const bool = moment(newAStr).isBefore(newBStr);

          return bool;
        },

        ellipsis: true,
      },
    ];

    return (
      <Card bordered={false}>
        <div className={styles.tableList}>
          <div>
            <NewFormSearch {...searchProps} />
          </div>
          <StandardTable
            rowKey="machineId"
            loading={loading}
            data={data}
            columns={columns}
            onChange={this.handleTableChange}
          />
        </div>
      </Card>
    );
  }
}

Index.propTypes = {};

export default Index;
